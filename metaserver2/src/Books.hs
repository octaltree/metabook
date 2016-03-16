{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Books where

import Models
import Data.Int (Int64)
import Data.List
import Data.Aeson
import Data.Aeson.TH

data Exclude = AndE Exclude Exclude | OrE Exclude Exclude | NotE Exclude
  | NoE | TitleE String | CircleE Int64 | WriterE Int64
  | PublisherE String | TagE String
  deriving (Show, Read, Eq)

$(deriveJSON defaultOptions ''Exclude)

toLeafNot :: Exclude -> Exclude
toLeafNot = toLeaf False
  where
    toLeaf :: Bool -> Exclude -> Exclude
    toLeaf True (NotE x) = toLeaf False x
    toLeaf False (NotE x) = toLeaf True x
    toLeaf True (AndE x y) = OrE (toLeaf True x) (toLeaf True y)
    toLeaf True (OrE x y) = AndE (toLeaf True x) (toLeaf True y)
    toLeaf False (AndE x y) = AndE (toLeaf False x) (toLeaf False y)
    toLeaf False (OrE x y) = OrE (toLeaf False x) (toLeaf False y)
    toLeaf True x = NotE x
    toLeaf False x = x

exceptByExclude :: [Book] -> Exclude -> [Book]
exceptByExclude bs ex = case toLeafNot ex of
  NoE -> bs
  TitleE str -> flip filter bs $ (any $ isInfixOf str) . book_titles
  CircleE idx -> flip filter bs $ (any (== idx)) . book_circles
  WriterE idx -> flip filter bs $ (any (== idx)) . book_writers
  PublisherE str -> flip filter bs $ (any $ isInfixOf str) . book_publishers
  TagE str -> flip filter bs $ (any $ isInfixOf str) . book_tags
  AndE x y -> intersect (exceptByExclude bs x) $ exceptByExclude bs y
  OrE x y -> union (exceptByExclude bs x) $ exceptByExclude bs y
  NotE (TitleE str) -> flip filter bs $ not . (any $ isInfixOf str) . book_titles
  NotE (CircleE idx) -> flip filter bs $ not . (any (== idx)) . book_circles
  NotE (WriterE idx) -> flip filter bs $ not . (any (== idx)) . book_writers
  NotE (PublisherE str) -> flip filter bs $ not . (any $ isInfixOf str) . book_publishers
  NotE (TagE str) -> flip filter bs $ not . (any $ isInfixOf str) . book_tags
