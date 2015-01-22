{-# LANGUAGE OverloadedStrings    #-}

import Database.Persist (insert, get)
import Database.Persist.Sqlite (runMigration, runSqlite)
import Control.Monad.IO.Class (liftIO)

import Model

main :: IO ()
main = runSqlite ":memory:" $ do
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
