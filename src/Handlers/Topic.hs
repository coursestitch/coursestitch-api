{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Handlers.Topic where

import Handlers.Handlers
import qualified Template
import Model.RunDB

topics :: RunDB -> ActionM ()
topics runDB = do
    topicList <- runDB getTopics
    template $ Template.topics topicList

topic :: RunDB -> ActionM ()
topic runDB = do
    title <- param "topic"
    topic <- runDB (getTopic title)
    case topic of Nothing                -> notFound404 "topic" 
                  Just (topic, concepts) -> template $ Template.topic topic concepts
