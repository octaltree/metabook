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
postBookH = undefined
getBookH :: Int -> EitherT ServantErr IO Book
getBookH = undefined
putBookH :: Int -> Book -> EitherT ServantErr IO ()
putBookH = undefined
deleteBookH :: Int -> EitherT ServantErr IO ()
deleteBookH = undefined

postCircleH :: Circle -> EitherT ServantErr IO Circle
postCircleH = undefined
getCircleH :: Int -> EitherT ServantErr IO Circle
getCircleH = undefined
putCircleH :: Int -> Circle -> EitherT ServantErr IO ()
putCircleH = undefined
deleteCircleH :: Int -> EitherT ServantErr IO ()
deleteCircleH = undefined

postWriterH :: Writer -> EitherT ServantErr IO Writer
postWriterH = undefined
getWriterH :: Int -> EitherT ServantErr IO Writer
getWriterH = undefined
putWriterH :: Int -> Writer -> EitherT ServantErr IO ()
putWriterH = undefined
deleteWriterH :: Int -> EitherT ServantErr IO ()
deleteWriterH = undefined

getAllTagsH :: EitherT ServantErr IO [String]
getAllTagsH = undefined

getAllWritersH :: EitherT ServantErr IO [Writer]
getAllWritersH = undefined
getAllCirclesH :: EitherT ServantErr IO [Circle]
getAllCirclesH = undefined
getAllBooksH :: EitherT ServantErr IO [Book]
getAllBooksH = undefined
