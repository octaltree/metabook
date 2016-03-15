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
Writer json
  body [String]
  UniqueWriterBody body
  deriving Show Eq
Circle json
  body [String]
  member [WriterId]
  UniqueCircleBody body
  deriving Show Eq
Book json
  titles [String]
  circles [CircleId]
  writers [WriterId]
  publishers [String]
  tags [String]
  UniqueBookTitles titles
  deriving Show Eq
|]
