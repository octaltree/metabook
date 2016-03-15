{-# LANGUAGE FlexibleContexts #-}
module RunDb where

import Models
import Dbm

import Servant
import Data.Int (Int64)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT, left)
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.Class as PC

create :: (Validatable at) => at -> EitherT ServantErr IO at
create new = do
  validate new
  runSqlite sqliteFile $ do
    entkey <- insert new
    ent <- get entkey
    case ent of
      Just n -> return n

read :: (Validatable at) => Key at -> EitherT ServantErr IO at
read key = do
  runSqlite sqliteFile $ do
    ent <- get key
    case ent of
      Nothing -> lift $ lift $ lift $ left err404
      Just o -> return o

update :: (Validatable at) => Key at -> at -> EitherT ServantErr IO ()
update key new = do
  validate new
  runSqlite sqliteFile $ do
    ent <- get key
    case ent of
      Nothing -> lift $ lift $ lift $ (left err404 :: EitherT ServantErr IO ())
      Just o -> replace key new

delete :: (Validatable at, ForeignStrict at) => Key at -> EitherT ServantErr IO ()
delete key = do
  foreignStrict key
  runSqlite sqliteFile $ PC.delete key
