{-# LANGUAGE OverloadedStrings #-}

module Model.TestData where

import Database.Persist (Entity, insert, get, entityVal)
import Database.Persist.Sql (ConnectionPool, runSqlPool, SqlPersistT)

import Model

testData :: SqlPersistT IO ()
testData = do
    -- Create dummy data.
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
    insert $ Relationship stringGuide Taught howToString
    return ()
