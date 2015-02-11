{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Main where

import Database.Persist.Sql (ConnectionPool, runSqlPool, runMigration)
import Database.Persist.Sqlite (withSqlitePool)
import Database.Persist.Postgresql (withPostgresqlPool)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import System.Environment (lookupEnv)
import Data.String (fromString)

import Network.Wai.Middleware.Static (staticPolicy, noDots, (>->), addBase)
import Web.Scotty (ScottyM, scotty, get, put, post, delete, middleware)

import CourseStitch (api)
import CourseStitch.Models (migrateAll)
import CourseStitch.Models.TestData (testData)
import CourseStitch.Models.RunDB

import qualified Handlers

main :: IO ()
main = do
    connStr <- lookupEnv "DATABASE_URL"
    let withPool = case connStr of
            Nothing -> withSqlitePool "coursestitch-api.db"
            Just cs -> withPostgresqlPool (fromString cs)
    runNoLoggingT $ withPool 2 $ \pool -> liftIO $ do
        runSqlPool (runMigration migrateAll >> testData) pool
        scotty 7000 (app (runDB' pool))

app :: RunDB -> ScottyM ()
app runDB = api runDB >> do
    get "/" $ Handlers.root runDB

    get "/resource/new" $ Handlers.resourceNew runDB
    get "/resource/:resource" $ Handlers.resourcePage runDB
    get "/resource/:resource/edit" $ Handlers.resourceEdit runDB

    get "/concept/new" $ Handlers.conceptNew runDB
    get "/concept/:concept" $ Handlers.conceptPage runDB
    get "/concept/:concept/edit" $ Handlers.conceptEdit runDB

    middleware $ staticPolicy (noDots >-> addBase "./static")
