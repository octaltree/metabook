{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Routing
  ( API
  ) where

import Models
import Data.Aeson
--import Data.Aeson.TH
import Servant

type BookEP = ()
type CircleEP = ()
type WriterEP = ()
type TagEP = ()

type API = "users" :> Get '[JSON] [Writer]
