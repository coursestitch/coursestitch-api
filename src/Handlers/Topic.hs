{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Handlers.Topic where

import Handlers.Handlers
import qualified Template
import Model.RunDB

topics :: RunDB -> ActionM ()
topics runDB = do
    topicList <- runDB getTopics
    template $ Template.topics topicList

topicNew :: RunDB -> ActionM ()
topicNew runDB = do
    template $ Template.topicForm Nothing

topicCreate :: RunDB -> ActionM ()
topicCreate runDB = do
    createdTopic <- topicFromParams

    topic <- runDB (newTopic createdTopic)
    case topic of
        Nothing    -> conflict409 "A topic with this URL already exists"
        Just topic -> template $ Template.topic topic []

topic :: RunDB -> ActionM ()
topic runDB = do
    title <- param "topic"
    topic <- runDB (getTopic title)
    case topic of Nothing                -> notFound404 "topic"
                  Just (topic, concepts) -> template $ Template.topic topic concepts

topicFromParams :: ActionM Topic
topicFromParams = do
    title    <- param "title"
    summary  <- param "summary"

    return $ Topic title summary
