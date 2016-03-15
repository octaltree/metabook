{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Books where

import Data.Int (Int64)
import Data.Aeson
import Data.Aeson.TH

data Exclude = And Exclude Exclude | Or Exclude Exclude | Not Exclude
  | TitleE String | CircleE Int64 | WriterE Int64
  | PublisherE String | TagE String
  deriving (Show, Read, Eq)

$(deriveJSON defaultOptions ''Exclude)

toLeafNot :: Exclude -> Exclude
toLeafNot e = toLeaf e False
  where
    toLeaf e notize = undefined
