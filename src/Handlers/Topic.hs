{-# LANGUAGE OverloadedStrings #-}

module Handlers.Topic where

import Handlers.Handlers
import qualified Template

topics :: ConnectionPool -> ActionM ()
topics pool = do
    topicList <- liftIO $ runSqlPool getTopics pool
    template $ Template.topics topicList

topic :: ConnectionPool -> ActionM ()
topic pool = do
    title <- param "topic"
    topic <- liftIO $ runSqlPool (getTopic title) pool
    case topic of Nothing                -> notFound404 "topic" 
                  Just (topic, concepts) -> template $ Template.topic topic concepts
