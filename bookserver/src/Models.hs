{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
module Models where

import Description

import Data.Int (Int64)
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sql
import Database.Persist.Sqlite
import Data.Aeson.TH
import Data.Time
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (EitherT, left)
import Servant
import Network.URI (isURI, isRelativeReference)
import Data.Maybe (isJust)

$(share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
BookAtT
  bookId Int64
  at Int64
  postAt UTCTime default=CURRENT_TIME
  UniqueBookAtAt at
  deriving Show Eq
AtT
  description Description
  path String
  deriving Show Eq
|])

sqliteFile = "test.sqlite"

data BookAt = BookAt {
  bookat_id :: Maybe Int64,
  bookat_book_id :: Int64,
  bookat_at :: Int64,
  bookat_post_at :: Maybe UTCTime
  } deriving (Show, Eq)

data At = At {
  at_id :: Maybe Int64,
  at_description :: Description,
  at_path :: String
  } deriving (Show, Eq)

$(deriveJSON defaultOptions ''BookAt)
$(deriveJSON defaultOptions ''At)

class (PersistEntity at, ToBackendKey SqlBackend at) => TableWrapper a at where
  key :: a -> Maybe Int64
  toTable :: a -> IO at
  fromTable :: at -> Int64 -> a

  fromEntity :: Entity at -> a
  fromEntity x = fromTable (entityVal x) (fromSqlKey $ entityKey x)

instance TableWrapper BookAt BookAtT where
  key = bookat_id
  toTable x = do
    now <- getCurrentTime
    return BookAtT {
      bookAtTBookId = bookat_book_id x,
      bookAtTAt = bookat_at x,
      bookAtTPostAt = now}
  fromTable x i = BookAt {
    bookat_id = Just i,
    bookat_book_id = bookAtTBookId x,
    bookat_at = bookAtTAt x,
    bookat_post_at = Just $ bookAtTPostAt x}

instance TableWrapper At AtT where
  key = at_id
  toTable x = return AtT {
    atTDescription = at_description x,
    atTPath = at_path x}
  fromTable x i = At {
    at_id = Just i,
    at_description = atTDescription x,
    at_path = atTPath x}

class (PersistEntity at, ToBackendKey SqlBackend at) => Validatable at where
  validate :: at -> EitherT ServantErr IO at

instance Validatable BookAtT where
  validate x = do
    ma <- (\a -> runSqlite sqliteFile $ do
      selectFirst [AtTId ==. (toSqlKey a)] []) (bookAtTAt x)
    if isJust ma then return x else left err400

instance Validatable AtT where
  validate x = case atTDescription x of
    Description.URI -> if isURI $ atTPath x then return x else left err400
    RelativePath -> if isRelativeReference $ atTPath x then return x else left err400
    Others -> return x

class (PersistEntity at, ToBackendKey SqlBackend at) => ForeignStrict at where
  foreignStrict :: Key at -> EitherT ServantErr IO ()

instance ForeignStrict BookAtT where
  foreignStrict _ = return ()

instance ForeignStrict AtT where
  foreignStrict key = do
    runSqlite sqliteFile $ do
      bas <- selectList ([] :: [Filter BookAtT]) []
      let candidates = map (toSqlKey . bookAtTAt . entityVal) bas
      if key `elem` candidates
        then lift $ lift $ lift $ left err400
        else return ()
