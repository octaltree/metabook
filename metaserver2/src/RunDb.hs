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

create :: (Validatable at) => at -> EitherT ServantErr IO at
create new = do
  validate new
  runSqlite sqliteFile $ do
    entkey <- insert new
    ent <- get entkey
    case ent of
      Just n -> return n

read :: (Validatable at) => Int64 -> EitherT ServantErr IO at
read idx = do
  let key = toSqlKey idx
  runSqlite sqliteFile $ do
    ent <- get key
    case ent of
      Nothing -> lift $ lift $ lift $ left err404
      Just o -> return o

update :: (Validatable at) => Int64 -> at -> EitherT ServantErr IO ()
update idx new = do
  validate new
  let key = toSqlKey idx
  runSqlite sqliteFile $ do
    ent <- get key
    case ent of
      Nothing -> lift $ lift $ lift $ (left err404 :: EitherT ServantErr IO ())
      Just o -> replace key new
