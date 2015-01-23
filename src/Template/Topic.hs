{-# LANGUAGE OverloadedStrings #-}

module Template.Topic where

import Data.Monoid (mconcat)

import Lucid
import Model

topics :: [Topic] -> Html ()
topics ts = ul_ $ mconcat $ map (li_ . topicSimple) ts

topic :: Topic -> [Concept] -> Html ()
topic topic concepts = article_ $ topicDetailed topic concepts

topicSimple :: Topic -> Html ()
topicSimple topic = do
    topicHeading topic
    topicText topic

topicDetailed :: Topic -> [Concept] -> Html ()
topicDetailed topic concepts = do
    topicHeading topic
    topicText topic
    p_ "No concepts"

topicHeading = h1_ . toHtml . topicTitle
topicText = p_ . toHtml . topicSummary
