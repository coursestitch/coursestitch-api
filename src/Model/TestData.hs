{-# LANGUAGE OverloadedStrings #-}

module Model.TestData where

import Database.Persist (Entity, insertUnique, get, entityVal)
import Database.Persist.Sql (ConnectionPool, runSqlPool, SqlPersistT)

import Model

testData :: SqlPersistT IO ()
testData = do
    -- Create dummy data.
    integerTopic <- insertUnique $ Topic "Integer" "A whole number"
    functionTopic <- insertUnique $ Topic "Function" "An operation that maps some input variables to some output variable"
    stringTopic <- insertUnique $ Topic "String" "A list of characters"
    howToString <- insertUnique $ Concept stringTopic "How to string"
    stringGuide <- insertUnique $ Resource {
        resourceTitle = "How does one string? All your questions answered.",
        resourceMedia = "text",
        resourceUrl   = "http://example.com/howtostring.html",
        resourceCourse = "Example.com Enterprise Programming Tutorials",
        resourceSummary = "This resource just says blah",
        resourcePreview = "blah blah blah",
        resourceKeywords = "strings, stringing, str"
     }
    case (stringGuide, howToString) of
        (Just resource, Just concept) -> insertUnique $ Relationship resource Taught concept
        otherwise -> return Nothing

    insertUnique $ User "sally" "$2y$04$d5rX41FWyMV2eNRcUxeZNuV50vvVjPyRlKQaNTd/ykJELJ38wGVWO"
    return ()
