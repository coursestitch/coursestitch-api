{-# LANGUAGE OverloadedStrings #-}

import Database.Persist.Sqlite (runSqlite)
import Database.Persist.Postgresql (withPostgresqlPool, runSqlPersistMPool)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import System.Environment (lookupEnv)
import Data.String (fromString)

import qualified Handlers

main :: IO ()
main = do
    connStr <- lookupEnv "DATABASE_URL"
    let runDB = case connStr of
            Nothing -> runSqlite ":memory:"
            Just cs -> runPostgresql (fromString cs)
    runDB Handlers.action

runPostgresql connStr action
    = runStdoutLoggingT $ withPostgresqlPool connStr 1 $ \pool ->
        liftIO $ flip runSqlPersistMPool pool $ action
