{-# LANGUAGE DataKinds, OverloadedStrings, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Main (main) where

import InitData
import Types

import Control.Monad.Trans.Either (EitherT, left)
import Data.List (sortBy)
import Data.Ord (comparing)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import qualified Data.IntMap.Lazy as IM (elems, lookup)
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


server :: Server VehicleAPI
server = getAllVehicles
    :<|> getVehicleById
    :<|> postVehicle
    :<|> putVehicle
    -----
    :<|> getIssuesById
    :<|> putIssues
  where
    getAllVehicles :: EitherT ServantErr IO [Vehicle]
    getAllVehicles = return . IM.elems $ vehicleTbl
    -- curl http://localhost:8081/vehicles/all

    getVehicleById :: Int -> EitherT ServantErr IO Vehicle
    getVehicleById = maybe notFound return . (`IM.lookup` vehicleTbl)
      where
        notFound = left err404 { errBody = "Vehicle ID not found." }
    -- curl http://localhost:8081/vehicles/0

    postVehicle :: Vehicle -> EitherT ServantErr IO Vehicle
    postVehicle v = do {- persist Vehicle with new database id -}
                       return v
    -- echo '{"year":2013,"model":"Void","issues":[{"issueType":"Electrical","priority":"High"}],"vin":"vin x"}' | curl -X POST -d @- http://localhost:8081/vehicles --header "Content-Type:application/json"

    putVehicle :: Int -> Vehicle -> EitherT ServantErr IO Vehicle
    putVehicle n v = do {- persist Vehicle with database id of 'n' -}
                        return v
    -- echo '{"year":2012,"model":"Iterate","issues":[{"issueType":"Battery","priority":"Low"}],"vin":"vin y"}' | curl -X PUT -d @- http://localhost:8081/vehicles/0 --header "Content-Type:application/json"

    getIssuesById :: Int -> Maybe SortBy -> EitherT ServantErr IO [Issue]
    getIssuesById n msb = do is <- issues <$> getVehicleById n
                             return $ maybe is (sortIssues is) msb
      where
        sortIssues :: [Issue] -> SortBy -> [Issue]
        sortIssues is sb = case sb of
            ByType     -> sortHelper issueType is
            ByPriority -> sortHelper priority is

        sortHelper :: Ord a => (Issue -> a) -> [Issue] -> [Issue]
        sortHelper = sortBy . comparing
    -- curl http://localhost:8081/vehicles/issues/1
    -- curl http://localhost:8081/vehicles/issues/1?sortBy=type
    -- curl http://localhost:8081/vehicles/issues/1?sortBy=priority

    putIssues :: Int -> [Issue] -> EitherT ServantErr IO [Issue]
    putIssues n is = do {- persist [Issue] for vehicle 'n' -}
                        return is
    -- echo '[{"issueType":"Powertrain","priority":"Low"}]' | curl -X PUT -d @- http://localhost:8081/vehicles/issues/1 --header "Content-Type:application/json"


-- Establish the web server.
vehicleAPI :: Proxy VehicleAPI
vehicleAPI = Proxy


-- "serve" comes from servant and hands you a WAI Application.
app :: Application
app = serve vehicleAPI server


main :: IO ()
main = run 8081 app
