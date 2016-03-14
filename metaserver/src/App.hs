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
  :<|> serverTag
  :<|> serverCircle
  :<|> serverWriter

serverBook :: Server BookEP
serverBook = handlerGetAllBooks
  :<|> handlerPostBook
  :<|> handlerGetBook
  :<|> handlerPutBook
  :<|> handlerDeleteBook

serverCircle :: Server CircleEP
serverCircle = handlerGetAllCircles
  :<|> handlerPostCircle
  :<|> handlerGetCircle
  :<|> handlerPutCircle
  :<|> handlerDeleteCircle

serverWriter :: Server WriterEP
serverWriter = handlerGetAllWriters
  :<|> handlerPostWriter
  :<|> handlerGetWriter
  :<|> handlerPutWriter
  :<|> handlerDeleteWriter

serverTag :: Server TagEP
serverTag = handlerGetAllTags

handlerGetAllBooks :: EitherT ServantErr IO [Book]
handlerGetAllBooks = undefined

handlerPostBook :: Book -> EitherT ServantErr IO Book
handlerPostBook bk = undefined

handlerGetBook :: Int -> EitherT ServantErr IO Book
handlerGetBook idx = runSqlite sqliteFile $ do
  bk <- selectFirst [BookId ==. (BookKey $ SqlBackendKey $ fromIntegral idx)] []
  case bk of
    Just b -> return $ entityVal b
    Nothing -> lift $ lift $ lift $ (left err404 :: EitherT ServantErr IO Book)

handlerPutBook :: Int -> Book -> EitherT ServantErr IO ()
handlerPutBook idx bk = undefined

handlerDeleteBook :: Int -> EitherT ServantErr IO ()
handlerDeleteBook idx = undefined

handlerGetAllCircles :: EitherT ServantErr IO [Circle]
handlerGetAllCircles = undefined

handlerPostCircle :: Circle -> EitherT ServantErr IO Circle
handlerPostCircle cr = undefined

handlerGetCircle :: Int -> EitherT ServantErr IO Circle
handlerGetCircle idx = runSqlite sqliteFile $ do
  cr <- selectFirst [CircleId ==. (CircleKey $ SqlBackendKey $ fromIntegral idx)] []
  case cr of
    Just c -> return $ entityVal c
    Nothing -> lift $ lift $ lift $ (left err404 :: EitherT ServantErr IO Circle)

handlerPutCircle :: Int -> Circle -> EitherT ServantErr IO ()
handlerPutCircle idx cr = undefined

handlerDeleteCircle :: Int -> EitherT ServantErr IO ()
handlerDeleteCircle idx = undefined

handlerGetAllWriters :: EitherT ServantErr IO [Writer]
handlerGetAllWriters = undefined

handlerPostWriter :: Writer -> EitherT ServantErr IO Writer
handlerPostWriter wr = undefined

handlerGetWriter :: Int -> EitherT ServantErr IO Writer
handlerGetWriter idx = runSqlite sqliteFile $ do
  wr <- selectFirst [WriterId ==. (WriterKey $ SqlBackendKey $ fromIntegral idx)] []
  case wr of
    Just w -> return $ entityVal w
    Nothing -> lift $ lift $ lift $ (left err404 :: EitherT ServantErr IO Writer)

handlerPutWriter :: Int -> Writer -> EitherT ServantErr IO ()
handlerPutWriter idx wr = undefined

handlerDeleteWriter :: Int -> EitherT ServantErr IO ()
handlerDeleteWriter idx = undefined

handlerGetAllTags :: EitherT ServantErr IO [String]
handlerGetAllTags = runSqlite sqliteFile $ do
  bks <- selectList [BookId !=. (BookKey $ SqlBackendKey $ -1)] []
  return $ nub $ concat $ map (bookTags . entityVal) bks
