{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TypeOperators   #-}
-- {-# LANGUAGE DataKinds       #-}
module App
  ( startApp
  ) where

import Models
import Routing
import Dbm
import RunDb

import Servant
import Data.Int (Int64)
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
server = bookS
  :<|> tagS
  :<|> circleS
  :<|> writerS

bookS :: Server BookEP
bookS = getAllBooksH
  :<|> postBookH
  :<|> getBookH
  :<|> putBookH
  :<|> deleteBookH

circleS :: Server CircleEP
circleS = getAllCirclesH
  :<|> postCircleH
  :<|> getCircleH
  :<|> putCircleH
  :<|> deleteCircleH

writerS :: Server WriterEP
writerS = getAllWritersH
  :<|> postWriterH
  :<|> getWriterH
  :<|> putWriterH
  :<|> deleteWriterH

tagS :: Server TagEP
tagS = getAllTagsH

postBookH :: Book -> EitherT ServantErr IO Book
postBookH = RunDb.create . (toTable :: Book -> BookT)
getBookH :: Int64 -> EitherT ServantErr IO Book
getBookH = RunDb.read . (toSqlKey :: Int64 -> Key BookT)
putBookH :: Int64 -> Book -> EitherT ServantErr IO ()
putBookH i n = RunDb.update (toSqlKey i) (toTable n :: BookT)
deleteBookH :: Int64 -> EitherT ServantErr IO ()
deleteBookH = RunDb.delete . (toSqlKey :: Int64 -> Key BookT)

postCircleH :: Circle -> EitherT ServantErr IO Circle
postCircleH = RunDb.create . (toTable :: Circle -> CircleT)
getCircleH :: Int64 -> EitherT ServantErr IO Circle
getCircleH = RunDb.read . (toSqlKey :: Int64 -> Key CircleT)
putCircleH :: Int64 -> Circle -> EitherT ServantErr IO ()
putCircleH i n = RunDb.update (toSqlKey i) (toTable n :: CircleT)
deleteCircleH :: Int64 -> EitherT ServantErr IO ()
deleteCircleH = RunDb.delete . (toSqlKey :: Int64 -> Key CircleT)

postWriterH :: Writer -> EitherT ServantErr IO Writer
postWriterH = RunDb.create . (toTable :: Writer -> WriterT)
getWriterH :: Int64 -> EitherT ServantErr IO Writer
getWriterH = RunDb.read . (toSqlKey :: Int64 -> Key WriterT)
putWriterH :: Int64 -> Writer -> EitherT ServantErr IO ()
putWriterH i n = RunDb.update (toSqlKey i) (toTable n :: WriterT)
deleteWriterH :: Int64 -> EitherT ServantErr IO ()
deleteWriterH = RunDb.delete . (toSqlKey :: Int64 -> Key WriterT)

getAllTagsH :: EitherT ServantErr IO [String]
getAllTagsH = runSqlite sqliteFile $ do
  bks <- selectList ([] :: [Filter BookT]) []
  return $ nub $ concat $ map (bookTTags . entityVal) bks
getAllPublishersH :: EitherT ServantErr IO [String]
getAllPublishersH = runSqlite sqliteFile $ do
  bks <- selectList ([] :: [Filter BookT]) []
  return $ nub $ concat $ map (bookTPublishers . entityVal) bks

getAllWritersH :: EitherT ServantErr IO [Writer]
getAllWritersH = runSqlite sqliteFile $ do
  wrs <- selectList ([] :: [Filter WriterT]) []
  return $ map fromEntity wrs
getAllCirclesH :: EitherT ServantErr IO [Circle]
getAllCirclesH = runSqlite sqliteFile $ do
  crs <- selectList ([] :: [Filter CircleT]) []
  return $ map fromEntity crs
getAllBooksH :: EitherT ServantErr IO [Book]
getAllBooksH = undefined
