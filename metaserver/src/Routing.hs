{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Routing where

import Models
import Data.Aeson
--import Data.Aeson.TH
import Servant

type BookEP = "books" :> Get '[JSON] [Book]
  :<|> "books" :> ReqBody '[JSON] Book :> Post '[JSON] Book
  :<|> "books" :> Capture "id" Int :> Get '[JSON] Book
  :<|> "books" :> Capture "id" Int :> ReqBody '[JSON] Book :> Put '[JSON] ()
  :<|> "books" :> Capture "id" Int :> Delete '[JSON] ()

type CircleEP = "circles" :> Get '[JSON] [Circle]
  :<|> "circles" :> ReqBody '[JSON] Circle :> Post '[JSON] Circle
  :<|> "circles" :> Capture "id" Int :> Get '[JSON] Circle
  :<|> "circles" :> Capture "id" Int :> ReqBody '[JSON] Circle :> Put '[JSON] ()
  :<|> "circles" :> Capture "id" Int :> Delete '[JSON] ()

type WriterEP = "writers" :> Get '[JSON] [Writer]
  :<|> "writers" :> ReqBody '[JSON] Writer :> Post '[JSON] Writer
  :<|> "writers" :> Capture "id" Int :> Get '[JSON] Writer
  :<|> "writers" :> Capture "id" Int :> ReqBody '[JSON] Writer :> Put '[JSON] Writer
  :<|> "writers" :> Capture "id" Int :> Delete '[JSON] ()

type TagEP = "tags" :> Get '[JSON] [String]

type API = BookEP
  :<|> CircleEP
  :<|> WriterEP
  :<|> TagEP
