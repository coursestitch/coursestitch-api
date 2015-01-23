{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.Persist.Sql (ConnectionPool, runSqlPool, runMigration)
import Database.Persist.Sqlite (withSqlitePool)
import Database.Persist.Postgresql (withPostgresqlPool)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import System.Environment (lookupEnv)
import Data.String (fromString)
import Web.Scotty (ScottyM, scotty, get)

import qualified Handlers
import Model (migrateAll)
import Model.TestData (testData)

main :: IO ()
main = do
    connStr <- lookupEnv "DATABASE_URL"
    let withPool = case connStr of
            Nothing -> withSqlitePool ":memory:"
            Just cs -> withPostgresqlPool (fromString cs)
    runNoLoggingT $ withPool 2 $ \pool -> liftIO $ do
        runSqlPool (runMigration migrateAll >> testData) pool
        scotty 7000 (app pool)

app :: ConnectionPool -> ScottyM ()
app pool = do
    get "/" $ Handlers.root pool
    get "/topic" $ Handlers.topics pool
    get "/topic/:topic" $ Handlers.topic pool
