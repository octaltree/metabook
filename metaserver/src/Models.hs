-- {-# LANGUAGE OverloadedStrings          #-}
-- {-# LANGUAGE EmptyDataDecls             #-}
-- {-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
module Models where

-- import Data.Aeson
-- import Data.Aeson.TH
-- import Database.Persist
import Database.Persist.TH
-- import Database.Persist.Sqlite
-- import Control.Monad.IO.Class (liftIO)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Book json
  titles [String]
  circles [CircleId]
  writers [WriterId]
  publishers [String]
  tags [String]
  deriving Show
Circle json
  body [String]
  member [WriterId]
  deriving Show
Writer json
  body [String]
  deriving Show
|]
