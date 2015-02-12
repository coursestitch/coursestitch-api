{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module CourseStitch.Handlers.Topic where

import Data.Int (Int64)

import CourseStitch.Handlers.Utils
import qualified CourseStitch.Templates as Templates
import CourseStitch.Models.RunDB

topics :: RunDB -> ActionM ()
topics runDB = do
    topicList <- runDB getTopics
    content topicList

topicCreate :: RunDB -> ActionM ()
topicCreate runDB = do
    createdTopic <- topicFromParams

    topic <- runDB (newTopic createdTopic)
    case topic of
        Nothing    -> conflict409 "A topic with this URL already exists"
        Just topic -> content topic

topic :: RunDB -> ActionM ()
topic runDB = do
    withTopicId $ \id -> do
        topic <- runDB (getTopic id)
        case topic of Nothing                -> notFound404 "topic"
                      Just (topic, concepts) -> content topic

withTopicId :: (Int64 -> ActionM ()) -> ActionM ()
withTopicId action = do
    id <- param "topic"
    case readMaybe id of
        Nothing -> badRequest400 "Topics should be of the form /topic/<integer>"
        Just id -> action id

topicFromParams :: ActionM Topic
topicFromParams = do
    title    <- param "title"
    summary  <- param "summary"

    return $ Topic title summary
