{-# LANGUAGE OverloadedStrings #-}

module Template.Topic where

import Lucid
import Model

import Template.Template
import Template.Concept

topics :: [Topic] -> Html ()
topics ts = unorderedList $ map topicSimple ts

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
    
    topicConceptsHeading
    case concepts of
        [] -> topicConceptsMissing
        concepts -> unorderedList $ map conceptSimple concepts

topicHeading = h1_ . toHtml . topicTitle
topicText = p_ . toHtml . topicSummary
topicConceptsHeading = h2_ "Concepts"
topicConceptsMissing = p_ "This topic has no concepts"
