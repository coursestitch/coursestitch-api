{-# LANGUAGE OverloadedStrings #-}

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
    get "/" $ Handlers.root

    get "/resource" $ Handlers.resources pool
    post "/resource" $ Handlers.resourceCreate pool
    get "/resource/new" $ Handlers.resourceNew pool
    get "/resource/:resource" $ Handlers.resource pool
    put "/resource/:resource" $ Handlers.resourceUpdate pool
    delete "/resource/:resource" $ Handlers.resourceDelete pool
    get "/resource/:resource/edit" $ Handlers.resourceEdit pool

    get "/concept" $ Handlers.concepts pool
    get "/concept/:concept" $ Handlers.concept pool

    get "/topic" $ Handlers.topics pool
    get "/topic/:topic" $ Handlers.topic pool

    get "/user" $ Handlers.users pool
    get "/user/:user" $ Handlers.user pool
    post "/login" $ Handlers.login pool
    delete "/login" $ Handlers.logout pool

    middleware $ staticPolicy (noDots >-> addBase "./static")
