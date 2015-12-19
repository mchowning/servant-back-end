{-# LANGUAGE DataKinds, DeriveGeneric, TypeFamilies, TypeOperators #-}

module Main where

import Data.Aeson
import Data.Time.Calendar
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

{-
data Admin = Admin
data Other = Other
data User  = User

data SortBy = Age | Name

type UserAPI = "admins"                               :> Get    '[JSON] [Admin]
          :<|> "others" :> Capture "id" Integer       :> Get    '[JSON] Other -- equivalent to 'GET /others/:id'
          :<|> "others" :> Capture "id" Integer       :> Delete '[]     ()
          :<|> "users"  :> QueryParam "sortby" SortBy :> Get    '[JSON] [User] -- equivalent to 'GET /users?sortby={age, name}'
          :<|> "users"  :> ReqBody '[JSON] User       :> Post   '[JSON] User
          :<|> Raw -- requests to anything else than /users go here, where the server will try to find a file with the right name at the right path
-}

data User = User
  { name  :: String
  , age   :: Int
  , email :: String
  , registration_date :: Day } deriving (Eq, Show, Generic)

{-
-- orphan ToJSON instance for Day. necessary to derive one for User
instance ToJSON Day where
  -- display a day in YYYY-mm-dd format
  toJSON d = toJSON (showGregorian d)
-}

instance ToJSON User
  where
    toJSON = genericToJSON defaultOptions

users :: [User]
users = [ albert, isaac ]

isaac :: User
isaac  = User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)

type UserAPI = 
       "users"  :> Get '[JSON] [User] -- By specifying "[User]" here, we get "EitherT ServantErr IO [User]".
  :<|> "albert" :> Get '[JSON] User
  :<|> "isaac"  :> Get '[JSON] User

server :: Server UserAPI
server =
       return users
  :<|> return albert
  :<|> return isaac

-- ==================================================
-- Using wai and warp, establish the web server.

userAPI :: Proxy UserAPI
userAPI = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: Application
app = serve userAPI server

main :: IO ()
main = run 8081 app -- http://localhost:8081/users or "curl http://localhost:8081/users"
