{-# LANGUAGE OverloadedStrings #-}
module App
  ( startApp
  ) where

import Models
import Routing

import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import Database.Persist
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either

sqliteFile = "test.sqlite"

startApp :: IO ()
startApp = do
  migrateModels
  run 8080 app

migrateModels :: IO ()
migrateModels = runSqlite sqliteFile $ runMigration migrateAll

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
serverTag = handlerGetAllTags

handlerGetAllBooks :: EitherT ServantErr IO [Book]
handlerGetAllBooks = return [Book [] [] [] [] [], Book [] [] [] [] []]

handlerGetBook :: Int -> EitherT ServantErr IO Book
handlerGetBook idx = return $ Book [] [] [] [] []

handlerGetAllTags :: EitherT ServantErr IO [String]
handlerGetAllTags = undefined
