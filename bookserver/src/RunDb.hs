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

create :: (TableWrapper a at, Validatable at) => at -> EitherT ServantErr IO a
create new = do
  validate new
  runSqlite sqliteFile $ do
    entkey <- insert new
    ent <- get entkey
    case ent of
      Just n -> return $ fromTable n (fromSqlKey entkey)

read :: (TableWrapper a at, Validatable at) => Key at -> EitherT ServantErr IO a
read key = do
  runSqlite sqliteFile $ do
    ent <- get key
    case ent of
      Nothing -> lift $ lift $ lift $ left err404
      Just o -> return $ fromTable o (fromSqlKey key)

update :: Validatable at => Key at -> at -> EitherT ServantErr IO ()
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
