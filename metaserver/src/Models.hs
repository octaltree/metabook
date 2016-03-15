-- {-# LANGUAGE EmptyDataDecls             #-}
-- {-# LANGUAGE FlexibleContexts           #-}
-- {-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
module Models where

import Servant
import Data.Maybe (isJust)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT, left)
-- import Data.Aeson
-- import Data.Aeson.TH
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sql
import Database.Persist.Sqlite
-- import Control.Monad.IO.Class (liftIO)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Writer json
  body [String]
  UniqueWriterBody body
  deriving Show Eq
Circle json
  body [String]
  writers [WriterId]
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

sqliteFile = "test.sqlite"

-- unique制約はdbが仕事してくれるから無視
class Validatable a where
  validate :: a -> EitherT ServantErr IO a


instance Validatable Writer where
  validate w
    | (== 0) . length . writerBody $ w = left err400
    | otherwise = return w

createWriter :: Writer -> EitherT ServantErr IO Writer
createWriter new = validate new >>= create
  where
    create n = runSqlite sqliteFile $ do
      entkey <- insert n
      ent <- get entkey
      case ent of
        Just c -> return c

readWriter :: Int -> EitherT ServantErr IO Writer
readWriter idx = runSqlite sqliteFile $ do
  mx <- selectFirst [WriterId ==. (WriterKey $ SqlBackendKey $ fromIntegral idx)] []
  case mx of
    Just x -> return $ entityVal x
    Nothing -> lift $ lift $ lift $ (left err404 :: EitherT ServantErr IO Writer)

updateWriter :: Int -> Writer -> EitherT ServantErr IO ()
updateWriter idx new = validate new >>= update idx
  where
    update :: Int -> Writer -> EitherT ServantErr IO ()
    update i n = runSqlite sqliteFile $ do
      mo <- selectFirst [WriterId ==. (WriterKey $ SqlBackendKey $ fromIntegral i)] []
      case mo of
        Nothing -> lift $ lift $ lift $ (left err404 :: EitherT ServantErr IO ())
        Just o -> replace (entityKey o) n

deleteWriter :: Int -> EitherT ServantErr IO ()
deleteWriter idx = undefined

instance Validatable Circle where
  validate c
    | (== 0) . length . circleBody $ c = left err400
    | (/= 0) . length . circleWriters $ c = do
      bools <- flip mapM (circleWriters c) $ \x -> runSqlite sqliteFile $ do
        undefined
      if all isJust bools then return c else left err400
    | otherwise = return c

createCircle :: Circle -> EitherT ServantErr IO Circle
createCircle new = validate new >>= create
  where
    create n = runSqlite sqliteFile $ do
      entkey <- insert n
      ent <- get entkey
      case ent of
        Just c -> return c

readCircle :: Int -> EitherT ServantErr IO Circle
readCircle idx = runSqlite sqliteFile $ do
  mx <- selectFirst [CircleId ==. (CircleKey $ SqlBackendKey $ fromIntegral idx)] []
  case mx of
    Just x -> return $ entityVal x
    Nothing -> lift $ lift $ lift (left err404 :: EitherT ServantErr IO Circle)

updateCircle :: Int -> Circle -> EitherT ServantErr IO ()
updateCircle idx new = validate new >>= update idx
  where
    update i n = runSqlite sqliteFile $ do
      mo <- selectFirst [CircleId ==. (CircleKey $ SqlBackendKey $ fromIntegral i)] []
      case mo of
        Nothing -> lift $ lift $ lift (left err404 :: EitherT ServantErr IO ())
        Just o -> replace (entityKey o) n

deleteCircle :: Int -> EitherT ServantErr IO ()
deleteCircle idx = undefined

instance Validatable Book where
  validate = undefined

createBook :: Book -> EitherT ServantErr IO Book
createBook new = validate new >>= create
  where
    create n = runSqlite sqliteFile $ do
      entkey <- insert n
      ent <- get entkey
      case ent of
        Just c -> return c

readBook :: Int -> EitherT ServantErr IO Book
readBook idx = runSqlite sqliteFile $ do
  mx <- selectFirst [BookId ==. (BookKey $ SqlBackendKey $ fromIntegral idx)] []
  case mx of
    Just x -> return $ entityVal x
    Nothing -> lift $ lift $ lift (left err404 :: EitherT ServantErr IO Book)

updateBook :: Int -> Book -> EitherT ServantErr IO ()
updateBook idx new = validate new >>= update idx
  where
    update i n = runSqlite sqliteFile $ do
      mo <- selectFirst [BookId ==. (BookKey $ SqlBackendKey $ fromIntegral i)] []
      case mo of
        Nothing -> lift $ lift $ lift (left err404 :: EitherT ServantErr IO ())
        Just o -> replace (entityKey o) n

deleteBook :: Int -> EitherT ServantErr IO ()
deleteBook idx = delete idx
  where
    delete i = runSqlite sqliteFile $ do
      deleteWhere [BookId ==. (BookKey $ SqlBackendKey $ fromIntegral i)]
