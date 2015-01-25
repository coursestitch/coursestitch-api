{-# LANGUAGE OverloadedStrings #-}

module Template.Topic where

import Data.Monoid (mappend)

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
    topicLink topic $ topicHeading topic
    topicText topic

topicDetailed :: Topic -> [Concept] -> Html ()
topicDetailed topic concepts = do
    topicLink topic $ topicHeading topic
    topicText topic
    
    topicConceptsHeading
    case concepts of
        [] -> topicConceptsMissing
        concepts -> unorderedList $ map conceptSimple concepts

topicUri topic = mappend "/topic/" (topicTitle topic)
topicLink topic html = link (topicUri topic) html

topicHeading = h1_ . toHtml . topicTitle
topicText = p_ . toHtml . topicSummary
topicConceptsHeading = h2_ "Concepts"
topicConceptsMissing = p_ "This topic has no concepts"
