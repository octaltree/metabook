{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Routing where

import Models

import Servant
import Data.Int (Int64)

type BookAtEP = "bookats" :> Get '[JSON] [BookAt]
  :<|> "bookats" :> ReqBody '[JSON] BookAt :> Post '[JSON] BookAt
  :<|> "bookats" :> Capture "id" Int64 :> Get '[JSON] BookAt
  :<|> "bookats" :> Capture "id" Int64 :> ReqBody '[JSON] BookAt :> Put '[JSON] ()
  :<|> "bookats" :> Capture "id" Int64 :> Delete '[JSON] ()

type BookEP = "books" :> Capture "id" Int64 :> "bookats" :> Get '[JSON] [BookAt]

type AtEP = "ats" :> Get '[JSON] [At]
  :<|> "ats" :> ReqBody '[JSON] At :> Post '[JSON] At
  :<|> "ats" :> Capture "id" Int64 :> Get '[JSON] At
  :<|> "ats" :> Capture "id" Int64 :> ReqBody '[JSON] At :> Put '[JSON] ()
  :<|> "ats" :> Capture "id" Int64 :> Delete '[JSON] ()

type API = BookAtEP
  :<|> AtEP
  :<|> BookEP
