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

import qualified Handlers
import Model (migrateAll)
import Model.TestData (testData)
import Model.RunDB

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
app runDB = do
    get "/" $ Handlers.root runDB

    get "/resource" $ Handlers.resources runDB
    post "/resource" $ Handlers.resourceCreate runDB
    get "/resource/new" $ Handlers.resourceNew runDB
    get "/resource/:resource" $ Handlers.resource runDB
    put "/resource/:resource" $ Handlers.resourceUpdate runDB
    delete "/resource/:resource" $ Handlers.resourceDelete runDB
    get "/resource/:resource/edit" $ Handlers.resourceEdit runDB

    get "/relationship" $ Handlers.relationships runDB
    get "/relationship/resource/:resource/:relationship/concept/:concept" $ Handlers.relationship runDB
    put "/relationship/resource/:resource/:relationship/concept/:concept" $ Handlers.relationshipCreate runDB
    delete "/relationship/resource/:resource/:relationship/concept/:concept" $ Handlers.relationshipDelete runDB

    get "/concept" $ Handlers.concepts runDB
    post "/concept" $ Handlers.conceptCreate runDB
    get "/concept/new" $ Handlers.conceptNew runDB
    get "/concept/:concept" $ Handlers.concept runDB
    put "/concept/:concept" $ Handlers.conceptUpdate runDB
    delete "/concept/:concept" $ Handlers.conceptDelete runDB
    get "/concept/:concept/edit" $ Handlers.conceptEdit runDB

    get "/topic" $ Handlers.topics runDB
    post "/topic" $ Handlers.topicCreate runDB
    get "/topic/new" $ Handlers.topicNew runDB
    get "/topic/:topic" $ Handlers.topic runDB

    get "/user" $ Handlers.users runDB
    get "/user/:user" $ Handlers.user runDB
    get "/session/new" $ Handlers.loginForm runDB
    post "/session" $ Handlers.login runDB
    delete "/session" $ Handlers.logout runDB

    get "/mastery" $ Handlers.masteries runDB -- For debugging only! Remove me!
    put "/mastery/resource/:resource" $ Handlers.resourceMasteryCreate runDB
    delete "/mastery/resource/:resource" $ Handlers.resourceMasteryDelete runDB
    put "/mastery/concept/:concept" $ Handlers.conceptMasteryCreate runDB
    delete "/mastery/concept/:concept" $ Handlers.conceptMasteryDelete runDB

    middleware $ staticPolicy (noDots >-> addBase "./static")
