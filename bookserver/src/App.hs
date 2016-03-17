module App
  ( startApp
  ) where

import Models
import RunDb
import Routing

import Servant
import Data.Int (Int64)
import Database.Persist.Sqlite
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Control.Monad.Trans.Either (EitherT, runEitherT, left)
import Control.Monad.Trans (liftIO)
import Control.Monad ((<=<))

startApp :: IO ()
startApp = do
  migrateModels
  run 8080 app

migrateModels :: IO ()
migrateModels = runSqlite sqliteFile $ runMigration migrateAll

app :: Application
app = simpleCors $ serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = bookAtS
  :<|> atS
  :<|> bookS

bookAtS :: Server BookAtEP
bookAtS = getAllBookAtsH
  :<|> postBookAtH
  :<|> getBookAtH
  :<|> putBookAtH
  :<|> deleteBookAtH

atS :: Server AtEP
atS = getAllAtsH
  :<|> postAtH
  :<|> getAtH
  :<|> putAtH
  :<|> deleteAtH

bookS :: Server BookEP
bookS = getAllBookAtsBookH

postBookAtH :: BookAt -> EitherT ServantErr IO BookAt
postBookAtH = RunDb.create <=< liftIO . (toTable :: BookAt -> IO BookAtT)
getBookAtH :: Int64 -> EitherT ServantErr IO BookAt
getBookAtH = RunDb.read . (toSqlKey :: Int64 -> Key BookAtT)
putBookAtH :: Int64 -> BookAt -> EitherT ServantErr IO ()
putBookAtH i n = do
  let key = toSqlKey i :: Key BookAtT
  b <- liftIO $ toTable n :: EitherT ServantErr IO BookAtT
  RunDb.update key b
deleteBookAtH :: Int64 -> EitherT ServantErr IO ()
deleteBookAtH = RunDb.delete . (toSqlKey :: Int64 -> Key BookAtT)
getAllBookAtsH :: EitherT ServantErr IO [BookAt]
getAllBookAtsH = runSqlite sqliteFile $ do
  xs <- selectList ([] :: [Filter BookAtT]) []
  return $ map fromEntity xs

postAtH :: At -> EitherT ServantErr IO At
postAtH = RunDb.create <=< liftIO . (toTable :: At -> IO AtT)
getAtH :: Int64 -> EitherT ServantErr IO At
getAtH = RunDb.read . (toSqlKey :: Int64 -> Key AtT)
putAtH :: Int64 -> At -> EitherT ServantErr IO ()
putAtH i n = do
  let key = toSqlKey i :: Key AtT
  a <- liftIO $ toTable n :: EitherT ServantErr IO AtT
  RunDb.update key a
deleteAtH :: Int64 -> EitherT ServantErr IO ()
deleteAtH = RunDb.delete . (toSqlKey :: Int64 -> Key AtT)
getAllAtsH :: EitherT ServantErr IO [At]
getAllAtsH = runSqlite sqliteFile $ do
  xs <- selectList ([] :: [Filter AtT]) []
  return $ map fromEntity xs

getAllBookAtsBookH :: Int64 -> EitherT ServantErr IO [BookAt]
getAllBookAtsBookH = undefined
