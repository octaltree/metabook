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
postBookAtH = undefined
getBookAtH :: Int64 -> EitherT ServantErr IO BookAt
getBookAtH = RunDb.read . (toSqlKey :: Int64 -> Key BookAtT)
putBookAtH :: Int64 -> BookAt -> EitherT ServantErr IO ()
putBookAtH i n = undefined
deleteBookAtH :: Int64 -> EitherT ServantErr IO ()
deleteBookAtH = RunDb.delete . (toSqlKey :: Int64 -> Key BookAtT)
getAllBookAtsH :: EitherT ServantErr IO [BookAt]
getAllBookAtsH = undefined

postAtH :: At -> EitherT ServantErr IO At
postAtH = undefined
getAtH :: Int64 -> EitherT ServantErr IO At
getAtH = undefined
putAtH :: Int64 -> At -> EitherT ServantErr IO ()
putAtH i n = undefined
deleteAtH :: Int64 -> EitherT ServantErr IO ()
deleteAtH = undefined
getAllAtsH :: EitherT ServantErr IO [At]
getAllAtsH = undefined

getAllBookAtsBookH :: Int64 -> EitherT ServantErr IO [BookAt]
getAllBookAtsBookH = undefined
