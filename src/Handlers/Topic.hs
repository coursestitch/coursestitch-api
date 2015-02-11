{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Handlers.Topic where

import CourseStitch.Handlers.Utils
import Templates

topicNew :: RunDB -> ActionM ()
topicNew runDB = do
    template $ Templates.topicForm Nothing
