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

handlerPostBook :: Book -> EitherT ServantErr IO Book
handlerPostBook = createBook
handlerGetBook :: Int -> EitherT ServantErr IO Book
handlerGetBook = readBook
handlerPutBook :: Int -> Book -> EitherT ServantErr IO ()
handlerPutBook = updateBook
handlerDeleteBook :: Int -> EitherT ServantErr IO ()
handlerDeleteBook = deleteBook

handlerPostCircle :: Circle -> EitherT ServantErr IO Circle
handlerPostCircle = createCircle
handlerGetCircle :: Int -> EitherT ServantErr IO Circle
handlerGetCircle = readCircle
handlerPutCircle :: Int -> Circle -> EitherT ServantErr IO ()
handlerPutCircle = updateCircle
handlerDeleteCircle :: Int -> EitherT ServantErr IO ()
handlerDeleteCircle = deleteCircle

handlerPostWriter :: Writer -> EitherT ServantErr IO Writer
handlerPostWriter = createWriter
handlerGetWriter :: Int -> EitherT ServantErr IO Writer
handlerGetWriter = readWriter
handlerPutWriter :: Int -> Writer -> EitherT ServantErr IO ()
handlerPutWriter = updateWriter
handlerDeleteWriter :: Int -> EitherT ServantErr IO ()
handlerDeleteWriter = deleteWriter

handlerGetAllTags :: EitherT ServantErr IO [String]
handlerGetAllTags = runSqlite sqliteFile $ do
  bks <- selectList ([] :: [Filter Book]) []
  return $ nub $ concat $ map (bookTags . entityVal) bks

handlerGetAllWriters :: EitherT ServantErr IO [Writer]
handlerGetAllWriters = undefined

handlerGetAllCircles :: EitherT ServantErr IO [Circle]
handlerGetAllCircles = undefined

handlerGetAllBooks :: EitherT ServantErr IO [Book]
handlerGetAllBooks = undefined
