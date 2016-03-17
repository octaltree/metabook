{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module RunDb where

import Description
import Models

import Servant
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.Class as PC
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (EitherT, left, runEitherT)

create :: (TableWrapper a, Validatable (Table a)) => (Table a) -> EitherT ServantErr IO a
create new = do
  validate new
  runSqlite sqliteFile $ do
    entkey <- insert new
    ent <- get entkey
    case ent of
      Just n -> return $ fromTable n (fromSqlKey entkey)

read :: (TableWrapper a, Validatable (Table a)) => Key (Table a) -> EitherT ServantErr IO a
read key = do
  runSqlite sqliteFile $ do
    ent <- get key
    case ent of
      Nothing -> lift $ lift $ lift $ left err404
      Just o -> return $ fromTable o (fromSqlKey key)

update :: (TableWrapper a, Validatable (Table a)) => Key (Table a) -> (Table a) -> EitherT ServantErr IO ()
update key new = do
  validate new
  runSqlite sqliteFile $ do
    ent <- get key
    case ent of
      Nothing -> lift $ lift $ lift $ (left err404 :: EitherT ServantErr IO ())
      Just o -> replace key new

delete :: (TableWrapper a, Validatable (Table a), ForeignStrict (Table a)) => Key (Table a) -> EitherT ServantErr IO ()
delete key = do
  foreignStrict key
  runSqlite sqliteFile $ PC.delete key
