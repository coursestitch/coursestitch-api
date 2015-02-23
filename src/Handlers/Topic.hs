{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Handlers.Topic where

import CourseStitch.Handlers.Utils
import qualified CourseStitch.Templates.Topic as Topic
import Templates

topics :: RunDB -> ActionM ()
topics runDB = do
    topicList <- runDB getTopics
    (template . page) $ Topic.topics topicList

topicNew :: RunDB -> ActionM ()
topicNew runDB = do
    template $ Templates.topicForm Nothing
