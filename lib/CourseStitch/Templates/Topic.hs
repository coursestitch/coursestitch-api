{-# LANGUAGE OverloadedStrings, FlexibleInstances, NoMonomorphismRestriction #-}

module CourseStitch.Templates.Topic where

import CourseStitch.Templates.Utils
import CourseStitch.Templates.Concept

instance ToHtml (Entity Topic) where
    toHtml = topicSimple

topics :: [Entity Topic] -> Html ()
topics ts = unorderedList $ map topicSimple ts

topic :: Entity Topic -> Html ()
topic topic = article_ $ topicSimple topic

topicSimple :: Monad m => Entity Topic -> HtmlT m ()
topicSimple topic = do
    topicLink topic $ topicHeading topic
    topicText topic

topicUri topic = mappend "/topic/" ((fromString . show . entityId) topic)
topicLink topic html = link (topicUri topic) html

topicHeading = h1_ . toHtml . topicTitle . entityVal
topicText = p_ . toHtml . topicSummary . entityVal
