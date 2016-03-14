
module App
  ( app
  ) where

import Models
import Routing

import Network.Wai
import Servant

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = undefined
