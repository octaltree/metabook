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

instance FromEntity WriterT Writer where
  fromEntity ent = let
    rawid = fromSqlKey . entityKey $ ent
    val = entityVal ent
    in return Writer {
      writer_id = Just rawid,
      writer_names = writerTNames val}

instance FromEntity CircleT Circle where
  fromEntity ent = let
    rawid = fromSqlKey . entityKey $ ent
    val = entityVal ent
    in return Circle {
      circle_id = Just rawid,
      circle_names = circleTNames val,
      circle_writers = circleTWriters val}

instance FromEntity BookT Book where
  fromEntity ent = let
    rawid = fromSqlKey . entityKey $ ent
    val = entityVal ent
    in return Book {
      book_id = Just rawid,
      book_titles = bookTTitles val,
      book_circles = bookTCircles val,
      book_writers = bookTWriters val,
      book_publishers = bookTPublishers val,
      book_tags = bookTTags val}

instance ToEntity WriterT Writer where
  toEntity kv = case writer_id kv of
    Nothing -> undefined
    Just idx -> undefined
