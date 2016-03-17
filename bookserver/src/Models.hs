{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
module Models
  ( Table
  , BookAt
  , At
  ) where

import Description

import Data.Int (Int64)
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sql
import Data.Aeson.TH
import Data.Time

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

type family Table a :: *
type instance Table BookAt = BookAtT
type instance Table At = AtT

class (PersistEntity (Table a), ToBackendKey SqlBackend (Table a)) => TableWrapper a where
  key :: a -> Maybe Int64
  toTable :: a -> IO (Table a)
  fromTable :: Table a -> Int64 -> a

  fromEntity :: Entity (Table a) -> a
  fromEntity x = fromTable (entityVal x) (fromSqlKey $ entityKey x)

instance TableWrapper BookAt where
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

instance TableWrapper At where
  key = at_id
  toTable x = return AtT {
    atTDescription = at_description x,
    atTPath = at_path x}
  fromTable x i = At {
    at_id = Just i,
    at_description = atTDescription x,
    at_path = atTPath x}
