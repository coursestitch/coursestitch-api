{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import Database.Persist (Entity, insert, get, entityVal)
import Database.Persist.Sql (ConnectionPool, runSqlPool, SqlPersistT)
import Database.Esqueleto (select, from, (^.), (?.), (==.), on, InnerJoin(..), just)

import Control.Monad.IO.Class (liftIO)
import Data.String (fromString)
import Web.Scotty (ActionM, text)

import Model

root :: ConnectionPool -> ActionM ()
root pool = do
    topics <- liftIO $ runSqlPool getTopics pool
    text (fromString . show $ topics)

getTopics :: SqlPersistT IO [Topic]
getTopics = do
    -- Select all concepts in the DB, and get the topic associated with them.
    topics <- select $
        from $ \(concept `InnerJoin` topic) -> do
        on (concept ^. ConceptTopic ==. just (topic ^. TopicId))
        return topic
    -- entityVal upwraps database entities.
    return $ map entityVal topics

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
