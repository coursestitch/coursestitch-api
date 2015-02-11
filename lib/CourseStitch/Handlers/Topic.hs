{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module CourseStitch.Handlers.Topic where

import CourseStitch.Handlers.Utils
import qualified CourseStitch.Templates as Templates
import CourseStitch.Models.RunDB

topics :: RunDB -> ActionM ()
topics runDB = do
    topicList <- runDB getTopics
    template $ Templates.topics topicList

topicCreate :: RunDB -> ActionM ()
topicCreate runDB = do
    createdTopic <- topicFromParams

    topic <- runDB (newTopic createdTopic)
    case topic of
        Nothing    -> conflict409 "A topic with this URL already exists"
        Just topic -> template $ Templates.topic topic

topic :: RunDB -> ActionM ()
topic runDB = do
    title <- param "topic"
    topic <- runDB (getTopic title)
    case topic of Nothing                -> notFound404 "topic"
                  Just (topic, concepts) -> template $ Templates.topic topic

topicFromParams :: ActionM Topic
topicFromParams = do
    title    <- param "title"
    summary  <- param "summary"

    return $ Topic title summary
