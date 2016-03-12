{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Control.Monad.IO.Class (liftIO)

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users" :> Get '[JSON] [User]
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] User
  :<|> "users" :> Capture "id" Int :> Get '[JSON] User
  :<|> "users" :> Capture "id" Int :> ReqBody '[JSON] User :> Put '[JSON] ()
  :<|> "users" :> Capture "id" Int :> Delete '[JSON] ()

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users
  :<|> (\x -> liftIO $ return $ head users)
  :<|> (return . \int -> head $ filter ((==int) . userId) users)
  :<|> (\id -> \obj -> liftIO $ print obj)
  :<|> (liftIO . print)

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
