-- {-# LANGUAGE EmptyDataDecls             #-}
-- {-# LANGUAGE FlexibleContexts           #-}
-- {-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
module Models where

import Servant
import Data.List (nub)
import Data.Maybe (isJust)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT, left)
import Data.Aeson
import Data.Aeson.TH
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.Types
-- import Control.Monad.IO.Class (liftIO)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
WriterT
  names [String]
  UniqueWriterNames names
  deriving Show Eq
CircleT
  names [String]
  writers [Int]
  UniqueCircleNames names
  deriving Show Eq
BookT
  titles [String]
  circles [Int]
  writers [Int]
  publishers [String]
  tags [String]
  UniqueBookTitles titles
  deriving Show Eq
|]

data Writer = Writer { writer_id :: Maybe Int, writer_names :: [String] } deriving (Show, Eq)
data Circle = Circle { circle_id :: Maybe Int, circle_names :: [String],
  circle_writers :: [Int] } deriving (Show, Eq)
data Book = Book { book_id :: Maybe Int, book_titles :: [String],
  book_circles :: [Int], book_writers :: [Int], book_publishers :: [String],
  book_tags :: [String] } deriving (Show, Eq)

$(deriveJSON defaultOptions ''Writer)
$(deriveJSON defaultOptions ''Circle)
$(deriveJSON defaultOptions ''Book)

class ToEntity at a where
  toEntity :: a -> EitherT ServantErr IO (Entity at)

class FromEntity at a where
  fromEntity :: Entity at -> EitherT ServantErr IO a
