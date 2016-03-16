{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Models
  ( Table
  , BookAt
  , Path
  )where

import Data.Int (Int64)
import Database.Persist.TH
import Data.Aeson.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
BookAtT
  bookId Int64
  at String Int64 Either
  deriving Show Eq
PathT
  path String String Either
  deriving Show Eq
|]

data BookAt = BookAt {
  bookat_id :: Int64,
  bookat_book_id :: Int64,
  bookat_at :: Either String Int64
  } deriving (Show, Eq)

data Path = Path {
  path_id :: Int64,
  path_path :: Either String String
  } deriving (Show, Eq)

$(deriveJSON defaultOptions ''BookAt)
$(deriveJSON defaultOptions ''Path)

type family Table a :: *
type instance Table BookAt = BookAtT
type instance Table Path = PathT
