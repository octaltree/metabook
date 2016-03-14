{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TypeOperators   #-}
-- {-# LANGUAGE DataKinds       #-}
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
import Control.Monad (join)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either
import Data.List (nub)

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
handlerGetBook idx = runSqlite sqliteFile $ do
  bk <- selectFirst [BookId ==. (BookKey $ SqlBackendKey $ fromIntegral idx)] []
  case bk of
    Just b -> return $ entityVal b
    Nothing -> lift $ lift $ lift $ (left err404 :: EitherT ServantErr IO Book)

handlerGetAllTags :: EitherT ServantErr IO [String]
handlerGetAllTags = runSqlite sqliteFile $ do
  bks <- selectList [BookId !=. (BookKey $ SqlBackendKey $ -1)] []
  return $ nub $ concat $ map (bookTags . entityVal) bks
  -- return ["hoge"]
