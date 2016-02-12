{-# LANGUAGE DataKinds, OverloadedStrings, TypeOperators #-}

module InMemoryDb (inMemoryDb) where

import InitData
import Types

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (EitherT, left)
import Data.Function (on)
import Data.IORef (IORef, atomicModifyIORef, newIORef, readIORef)
import Data.List ((\\), sortBy)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import qualified Data.IntMap.Lazy as IM (IntMap, elems, insert, keys, lookup)
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
server ior = getAllVehicles
        :<|> getVehicleById
        :<|> postVehicle
        :<|> putVehicle
        -----
        :<|> getIssuesById
        :<|> putIssues
  where
    getAllVehicles :: EitherT ServantErr IO [Vehicle]
    getAllVehicles = IM.elems <$> liftIO (readIORef ior)
    -- curl http://localhost:8081/vehicles/all

    getVehicleById :: Int -> EitherT ServantErr IO Vehicle
    getVehicleById = getByIdHelper id
    -- curl http://localhost:8081/vehicles/0

    getByIdHelper f i = maybe oops (return . f) . IM.lookup i =<< liftIO (readIORef ior)
    oops              = left err404 { errBody = "Vehicle ID not found." }

    postVehicle :: Vehicle -> EitherT ServantErr IO Vehicle
    postVehicle v = liftIO . atomicModifyIORef ior $ f
      where
        f tbl = let newId = head . ([0..] \\) . IM.keys $ tbl
                    v'    = v { dbId = Just newId }
                in (IM.insert newId v' tbl, v')
    -- echo '{"year":2013,"model":"Void","issues":[{"issueType":"Electrical","priority":"High"}],"vin":"vin x"}' | curl -X POST -d @- http://localhost:8081/vehicles --header "Content-Type:application/json"

    putVehicle :: Int -> Vehicle -> EitherT ServantErr IO Vehicle
    putVehicle i v = putHelper f
      where
        f tbl = maybe (tbl, Nothing) (const found) . IM.lookup i $ tbl
          where
            found = let v' = v { dbId = Just i }
                    in (IM.insert i v' tbl, Just v')
    -- echo '{"year":2012,"model":"Iterate","issues":[{"issueType":"Exhaust","priority":"Low"}],"vin":"vin y"}' | curl -X PUT -d @- http://localhost:8081/vehicles/0 --header "Content-Type:application/json"

    -- A good exercise would be to see if we can add more to this function.
    putHelper f = maybe oops return =<< liftIO (atomicModifyIORef ior f)

    getIssuesById :: Int -> Maybe SortBy -> EitherT ServantErr IO [Issue]
    getIssuesById i = maybe (getByIdHelper issues i) sortIssues
      where
        sortIssues how = flip getByIdHelper i $ case how of
            ByType     -> sortHelper issueType
            ByPriority -> sortHelper priority
        sortHelper f = sortBy (compare `on` f) . issues
    -- curl http://localhost:8081/vehicles/issues/1
    -- curl http://localhost:8081/vehicles/issues/1?sortBy=type
    -- curl http://localhost:8081/vehicles/issues/1?sortBy=priority

    putIssues :: Int -> [Issue] -> EitherT ServantErr IO [Issue]
    putIssues i is = putHelper f
      where
        f tbl = maybe (tbl, Nothing) found . IM.lookup i $ tbl
          where
            found v = let v' = v { issues = is }
                      in (IM.insert i v' tbl, Just is)
    -- echo '[{"issueType":"Powertrain","priority":"Low"}]' | curl -X PUT -d @- http://localhost:8081/vehicles/issues/1 --header "Content-Type:application/json"


-- Establish the web server.
vehicleAPI :: Proxy VehicleAPI
vehicleAPI = Proxy


-- "serve" comes from servant and hands you a WAI Application.
app :: IORef (IM.IntMap Vehicle) -> Application
app = serve vehicleAPI . server


inMemoryDb :: IO ()
inMemoryDb = run 8081 . app =<< newIORef vehicleTbl
