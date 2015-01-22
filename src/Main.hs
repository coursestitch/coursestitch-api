{-# LANGUAGE OverloadedStrings #-}

import Database.Persist (insert, get)
import Database.Persist.Sql (runMigration)
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.Postgresql (withPostgresqlPool, runSqlPersistMPool)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import System.Environment (getArgs)

import Model

main :: IO ()
main = do
    args <- getArgs
    (if elem "pg" args
        then runPostgresql ""
        else runSqlite ":memory:") action

action = do
    runMigration migrateAll
    stringTopic <- insert $ Topic "String" ""
    howToString <- insert $ Concept (Just stringTopic) "How to string"
    stringGuide <- insert $ Resource {
        resourceTitle = "How does one string? All your questions answered.",
        resourceMedia = "text",
        resourceUrl   = "http://example.com/howtostring.html",
        resourceCourse = "Example.com Enterprise Programming Tutorials",
        resourceSummary = "blah blah blah",
        resourcePreview = "blah blah blah",
        resourceKeywords = "strings, stringing, str"
     }
    string <- get stringGuide
    liftIO $ print string

runPostgresql connStr action
    = runStdoutLoggingT $ withPostgresqlPool connStr 1 $ \pool ->
        liftIO $ flip runSqlPersistMPool pool $ action
