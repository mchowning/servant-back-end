{-# LANGUAGE DataKinds, OverloadedStrings, TypeOperators #-}

module Main where

import Types
import InitData

import Control.Monad.Trans.Either
import Network.Wai
import Network.Wai.Handler.Warp
import Servant


type DiscAPI = "all"                        :> Get '[JSON] [Artist]
          :<|> "artist" :> Capture "id" Int :> Get '[JSON] Artist


server :: Server DiscAPI
server = handlerAll
    :<|> handlerArtist
  where
    handlerAll :: EitherT ServantErr IO [Artist]
    handlerAll = return artists

    handlerArtist :: Int -> EitherT ServantErr IO Artist
    handlerArtist i = return $ artists !! i


-- Establish the web server.
discAPI :: Proxy DiscAPI
discAPI = Proxy


-- "serve" comes from servant and hands you a WAI Application.
app :: Application
app = serve discAPI server


main :: IO ()
main = run 8081 app -- http://localhost:8081/all or "curl http://localhost:8081/all"


{-
type UserAPI = "admins"                               :> Get    '[JSON] [Admin]
          :<|> "others" :> Capture "id" Integer       :> Get    '[JSON] Other -- equivalent to 'GET /others/:id'
          :<|> "others" :> Capture "id" Integer       :> Delete '[]     ()
          :<|> "users"  :> QueryParam "sortby" SortBy :> Get    '[JSON] [User] -- equivalent to 'GET /users?sortby={age, name}'
          :<|> "users"  :> ReqBody '[JSON] User       :> Post   '[JSON] User
          :<|> Raw -- requests to anything else than /users go here, where the server will try to find a file with the right name at the right path
-}
