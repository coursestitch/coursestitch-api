{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import Control.Monad.IO.Class (liftIO)
import Database.Persist (insert, get)
import Database.Persist.Sql (runMigration, SqlPersistM)
import Database.Esqueleto (select, from, (^.), (?.), (==.), on, InnerJoin(..), just)

import Model

action :: SqlPersistM ()
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
    topics <- select $
        from $ \(concept `InnerJoin` topic) -> do
        on (concept ^. ConceptTopic ==. just (topic ^. TopicId))
        return topic
    liftIO $ print topics
