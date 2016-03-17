{-# LANGUAGE TemplateHaskell            #-}
module Description where

import Database.Persist.TH
import Data.Aeson.TH

data Description = URI | RelativePath | Others deriving (Show, Eq, Read)
$(derivePersistField "Description")
$(deriveJSON defaultOptions ''Description)
