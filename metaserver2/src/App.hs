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
getAllTagsH = undefined

getAllWritersH :: EitherT ServantErr IO [Writer]
getAllWritersH = undefined
getAllCirclesH :: EitherT ServantErr IO [Circle]
getAllCirclesH = undefined
getAllBooksH :: EitherT ServantErr IO [Book]
getAllBooksH = undefined
