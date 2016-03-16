module App
  ( startApp
  ) where

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)

startApp :: IO ()
startApp = do
  run 8080 app

app :: Application
app = simpleCors $ undefined
