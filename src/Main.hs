{-# LANGUAGE DataKinds, OverloadedStrings, TypeOperators #-}

module Main (main) where

import InitData
import Types

import Control.Monad.Trans.Either (EitherT, left)
import Data.Function (on)
import Data.List ((\\), sortBy)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import qualified Data.IntMap.Lazy as IM (elems, keys, lookup)
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
    getVehicleById = byIdHelper id
    -- curl http://localhost:8081/vehicles/0

    byIdHelper f = maybe notFound (return . f) . (`IM.lookup` vehicleTbl)
      where
        notFound = left err404 { errBody = "Vehicle ID not found." }

    postVehicle :: Vehicle -> EitherT ServantErr IO Vehicle
    postVehicle v = let newId = head . ([0..] \\) . IM.keys $ vehicleTbl
                    in return v { dbId = Just newId }
    -- echo '{"year":2013,"model":"Void","issues":[{"issueType":"Electrical","priority":"High"}],"vin":"vin x"}' | curl -X POST -d @- http://localhost:8081/vehicles --header "Content-Type:application/json"

    putVehicle :: Int -> Vehicle -> EitherT ServantErr IO Vehicle
    putVehicle i = flip byIdHelper i . const
    -- echo '{"year":2012,"model":"Iterate","issues":[{"issueType":"Exhaust","priority":"Low"}],"vin":"vin y"}' | curl -X PUT -d @- http://localhost:8081/vehicles/0 --header "Content-Type:application/json"

    getIssuesById :: Int -> Maybe SortBy -> EitherT ServantErr IO [Issue]
    getIssuesById i = maybe (byIdHelper issues i) sortIssues
      where
        sortIssues how = flip byIdHelper i $ case how of
            ByType     -> sortHelper issueType
            ByPriority -> sortHelper priority
        sortHelper f = sortBy (compare `on` f) . issues
    -- curl http://localhost:8081/vehicles/issues/1
    -- curl http://localhost:8081/vehicles/issues/1?sortBy=type
    -- curl http://localhost:8081/vehicles/issues/1?sortBy=priority

    putIssues :: Int -> [Issue] -> EitherT ServantErr IO [Issue]
    putIssues i = flip byIdHelper i . const
    -- echo '[{"issueType":"Powertrain","priority":"Low"}]' | curl -X PUT -d @- http://localhost:8081/vehicles/issues/1 --header "Content-Type:application/json"


-- Establish the web server.
vehicleAPI :: Proxy VehicleAPI
vehicleAPI = Proxy


-- "serve" comes from servant and hands you a WAI Application.
app :: Application
app = serve vehicleAPI server


main :: IO ()
main = run 8081 app
