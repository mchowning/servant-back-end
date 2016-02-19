{-# LANGUAGE DataKinds, OverloadedStrings, TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module InMemoryDb (inMemoryDb) where

import InitData
import Types

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Either (EitherT, left)
import Data.IORef (IORef, atomicModifyIORef, newIORef, readIORef)
import Data.List ((\\), sortBy)
import Data.Ord (comparing)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import qualified Data.IntMap.Lazy as IM (IntMap, elems, insert, keys, lookup, member)
import Servant


type VehicleAPI =
       "vehicles" :> "all"                                       :> Get  '[JSON] [Vehicle]
  :<|> "vehicles" :> Capture "id" Int                            :> Get  '[JSON] Vehicle
  :<|> "vehicles" :>                     ReqBody '[JSON] Vehicle :> Post '[JSON] Vehicle
  :<|> "vehicles" :> Capture "id" Int :> ReqBody '[JSON] Vehicle :> Put  '[JSON] Vehicle
  -----
  :<|> "vehicles" :> "issues" :> Capture "id" Int :> QueryParam "sortBy" SortBy :> Get '[JSON] [Issue]
  :<|> "vehicles" :> "issues" :> Capture "id" Int :> ReqBody '[JSON] [Issue]    :> Put '[JSON] [Issue]


data SortBy = ByType | ByPriority


instance FromText SortBy where
    fromText "type"     = Just ByType
    fromText "priority" = Just ByPriority
    fromText _          = Nothing


instance ToText SortBy where
    toText ByType     = "type"
    toText ByPriority = "priority"


server :: IORef (IM.IntMap Vehicle) -> Server VehicleAPI
-- FIXME get rid of all these ior's
server ior = getAllVehicles ior
        :<|> getVehicleById ior
        :<|> postVehicle ior
        :<|> putVehicle ior
        -----
        :<|> getIssuesById ior
        :<|> putIssues ior

getAllVehicles :: IORef (IM.IntMap Vehicle) -> EitherT ServantErr IO [Vehicle]
getAllVehicles ior = IM.elems <$> liftIO (readIORef ior)
-- curl http://localhost:8081/vehicles/all

getVehicleById :: IORef (IM.IntMap Vehicle) -> Int -> EitherT ServantErr IO Vehicle
getVehicleById ior i = maybe oops return . IM.lookup i =<< mIOTableRef ior
-- curl http://localhost:8081/vehicles/0

oops :: EitherT ServantErr IO a
oops = left err404 { errBody = "Vehicle ID not found." }

--     lifter :: Monad m => IORef a -> m a
mIOTableRef :: MonadIO m => IORef (IM.IntMap Vehicle) -> m (IM.IntMap Vehicle)
mIOTableRef = liftIO . readIORef

postVehicle :: IORef (IM.IntMap Vehicle) -> Vehicle -> EitherT ServantErr IO Vehicle
postVehicle ior v = liftIO . atomicModifyIORef ior $ insert
  where
    insert :: IM.IntMap Vehicle -> (IM.IntMap Vehicle, Vehicle)
    insert tbl = let newUniqueId = head . ([0..] \\) . IM.keys $ tbl
                     updatedTbl = IM.insert newUniqueId v tbl
                 in (updatedTbl, v)
-- echo '{"year":2013,"model":"Void","issues":[{"issueType":"Electrical","priority":"High"}],"vin":"vin x"}' | curl -X POST -d @- http://localhost:8081/vehicles --header "Content-Type:application/json"

putVehicle :: IORef (IM.IntMap Vehicle) -> Int -> Vehicle -> EitherT ServantErr IO Vehicle
putVehicle ior i v = putHelper ior f
  where
    f :: IM.IntMap Vehicle -> (IM.IntMap Vehicle, Maybe Vehicle)
    f tbl = if IM.member i tbl
            then (updatedTbl, Just v)
            else (tbl, Nothing)
      where
        updatedTbl = IM.insert i v tbl
-- echo '{"year":2012,"model":"Iterate","issues":[{"issueType":"Brakes","priority":"Low"}],"vin":"vin y"}' | curl -X PUT -d @- http://localhost:8081/vehicles/0 --header "Content-Type:application/json"

-- A good exercise would be to see if we can add more to this function.
putHelper :: forall a.
             IORef (IM.IntMap Vehicle)
          -> (IM.IntMap Vehicle -> (IM.IntMap Vehicle, Maybe a))
          -> EitherT ServantErr IO a
putHelper ior f = maybe oops return =<< liftIO putResult
  where
    putResult :: IO (Maybe a)
    putResult = atomicModifyIORef ior f

getIssuesById :: IORef (IM.IntMap Vehicle) -> Int -> Maybe SortBy -> EitherT ServantErr IO [Issue]
getIssuesById ior i msb = do unsorted <- issues <$> getVehicleById ior i
                             return $ maybe unsorted (sortIssues unsorted) msb
  where
    sortIssues :: [Issue] -> SortBy -> [Issue]
    sortIssues is how = case how of
        ByType     -> sortHelper issueType is
        ByPriority -> sortHelper priority is
    sortHelper :: Ord b => (a -> b) -> [a] -> [a]
    sortHelper = sortBy . comparing
-- curl http://localhost:8081/vehicles/issues/1
-- curl http://localhost:8081/vehicles/issues/1?sortBy=type
-- curl http://localhost:8081/vehicles/issues/1?sortBy=priority

putIssues :: IORef (IM.IntMap Vehicle) -> Int -> [Issue] -> EitherT ServantErr IO [Issue]
putIssues ior i is = putHelper ior f
  where
    f :: IM.IntMap Vehicle -> (IM.IntMap Vehicle, Maybe [Issue])
    f tbl = maybe (tbl, Nothing) found (IM.lookup i tbl)
      where
        found :: Vehicle -> (IM.IntMap Vehicle, Maybe [Issue])
        found v = let v' = v { issues = is }
                      updatedTbl = IM.insert i v' tbl
                  in (updatedTbl, Just is)
-- echo '[{"issueType":"Electrical","priority":"Low"}]' | curl -X PUT -d @- http://localhost:8081/vehicles/issues/1 --header "Content-Type:application/json"


-- Establish the web server.
vehicleAPI :: Proxy VehicleAPI
vehicleAPI = Proxy


-- "serve" comes from servant and hands you a WAI Application.
app :: IORef (IM.IntMap Vehicle) -> Application
app = serve vehicleAPI . server


inMemoryDb :: IO ()
inMemoryDb = do ior <- newIORef vehicleTbl
                run 8081 (app ior)
