{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Routing where

import Models
import Servant
import Data.Int (Int64)

type BookEP = "books" :> Get '[JSON] [Book]
  :<|> "books" :> ReqBody '[JSON] Book :> Post '[JSON] Book
  :<|> "books" :> Capture "id" Int64 :> Get '[JSON] Book
  :<|> "books" :> Capture "id" Int64 :> ReqBody '[JSON] Book :> Put '[JSON] ()
  :<|> "books" :> Capture "id" Int64 :> Delete '[JSON] ()

type CircleEP = "circles" :> Get '[JSON] [Circle]
  :<|> "circles" :> ReqBody '[JSON] Circle :> Post '[JSON] Circle
  :<|> "circles" :> Capture "id" Int64 :> Get '[JSON] Circle
  :<|> "circles" :> Capture "id" Int64 :> ReqBody '[JSON] Circle :> Put '[JSON] ()
  :<|> "circles" :> Capture "id" Int64 :> Delete '[JSON] ()

type WriterEP = "writers" :> Get '[JSON] [Writer]
  :<|> "writers" :> ReqBody '[JSON] Writer :> Post '[JSON] Writer
  :<|> "writers" :> Capture "id" Int64 :> Get '[JSON] Writer
  :<|> "writers" :> Capture "id" Int64 :> ReqBody '[JSON] Writer :> Put '[JSON] ()
  :<|> "writers" :> Capture "id" Int64 :> Delete '[JSON] ()

type TagEP = "tags" :> Get '[JSON] [String]

type API = BookEP
  :<|> TagEP
  :<|> CircleEP
  :<|> WriterEP
