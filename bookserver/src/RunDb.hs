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
import Control.Monad.Trans.Either (EitherT, left, runEitherT)

create :: (TableWrapper a, Validatable (Table a)) => (Table a) -> EitherT ServantErr IO a
create new = do
  validate new
  runSqlite sqliteFile $ do
    entkey <- insert new
    ent <- get entkey
    case ent of
      Just n -> return $ fromTable n (fromSqlKey entkey)
