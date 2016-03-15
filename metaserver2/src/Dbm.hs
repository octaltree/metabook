-- {-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
-- {-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
module Dbm where

import Models

import Servant
import Data.Int (Int64)
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

class ToTable at a where
  toTable :: a -> at

class (ToBackendKey SqlBackend at) => FromTable at a where
  fromTable :: at -> Int64 -> a
  fromEntity :: Entity at -> a
  fromEntity ent = fromTable (entityVal ent) (fromSqlKey $ entityKey $ ent)

class (ToBackendKey SqlBackend at) => Validatable at where
  validate :: at -> EitherT ServantErr IO at

instance FromTable WriterT Writer where
  fromTable t i = Writer {
    writer_id = Just i,
    writer_names = writerTNames t}

instance FromTable CircleT Circle where
  fromTable t i = Circle {
    circle_id = Just i,
    circle_names = circleTNames t,
    circle_writers = circleTWriters t}

instance FromTable BookT Book where
  fromTable t i = Book {
    book_id = Just i,
    book_titles = bookTTitles t,
    book_circles = bookTCircles t,
    book_writers = bookTWriters t,
    book_publishers = bookTPublishers t,
    book_tags = bookTTags t}

instance ToTable WriterT Writer where
  toTable x = WriterT {
    writerTNames = writer_names x}

instance ToTable CircleT Circle where
  toTable x = CircleT {
    circleTNames = circle_names x,
    circleTWriters = circle_writers x}

instance ToTable BookT Book where
  toTable x = BookT {
    bookTTitles = book_titles x,
    bookTCircles = book_circles x,
    bookTWriters = book_writers x,
    bookTPublishers = book_publishers x,
    bookTTags = book_tags x}

lengthNotZero :: a -> (a -> [b]) -> EitherT ServantErr IO a
lengthNotZero t f
  | (== 0) . length . f $ t = left err400
  | otherwise = return t

instance Validatable WriterT where
  validate w = lengthNotZero w writerTNames

instance Validatable CircleT where
  validate c = do
    lengthNotZero c circleTNames
    let ws = circleTWriters c
    ms <- flip mapM ws $ \x -> runSqlite sqliteFile $ do
      selectFirst [WriterTId ==. (toSqlKey x)] []
    if all isJust ms then return c else left err400

instance Validatable BookT where
  validate b = do
    lengthNotZero b bookTTitles
    let ws = bookTWriters b
    mws <- flip mapM ws $ \x -> runSqlite sqliteFile $ do
      selectFirst [WriterTId ==. (toSqlKey x)] []
    if all isJust mws then return b else left err400
    let cs = bookTCircles b
    mcs <- flip mapM cs $ \x -> runSqlite sqliteFile $ do
      selectFirst [CircleTId ==. (toSqlKey x)] []
    if all isJust mcs then return b else left err400
