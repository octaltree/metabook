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

$(share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
BookAtT
  bookId Int64
  at Int64
  deriving Show Eq
AtT
  description Description
  path String
  deriving Show Eq
|])

data BookAt = BookAt {
  bookat_id :: Int64,
  bookat_book_id :: Int64,
  bookat_at :: Int64
  } deriving (Show, Eq)

data At = At {
  at_id :: Int64,
  at_description :: Description,
  at_path :: String
  } deriving (Show, Eq)

$(deriveJSON defaultOptions ''BookAt)
$(deriveJSON defaultOptions ''At)

type family Table a :: *
type instance Table BookAt = BookAtT
type instance Table At = AtT

class (PersistEntity (Table a), ToBackendKey SqlBackend (Table a)) => TableWrapper a where
  key :: a -> Int64
  toTable :: a -> Table a
  fromTable :: Table a -> Int64 -> a

  toEntity :: a -> Entity (Table a)
  toEntity x = Entity (toSqlKey $ key x) (toTable x)
  fromEntity :: Entity (Table a) -> a
  fromEntity x = fromTable (entityVal x) (fromSqlKey $ entityKey x)
