
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
server = serverBook
  :<|> serverCircle
  :<|> serverWriter
  :<|> serverTag

serverBook :: Server BookEP
serverBook = handlerGetAllBooks
  :<|> undefined
  :<|> handlerGetBook
  :<|> undefined
  :<|> undefined

serverCircle :: Server CircleEP
serverCircle = undefined

serverWriter :: Server WriterEP
serverWriter = undefined

serverTag :: Server TagEP
serverTag = undefined

handlerGetAllBooks = return [Book [] [] [] [] []]

handlerGetBook idx = return $ Book [] [] [] [] []
