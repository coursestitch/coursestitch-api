{-# LANGUAGE OverloadedStrings #-}

module Template.Topic where

import Data.Monoid (mappend)

import Lucid
import Model
import Database.Persist (Entity, entityVal)

import Template.Template
import Template.Concept

topics :: [Entity Topic] -> Html ()
topics ts = unorderedList $ map topicSimple ts

topic :: Entity Topic -> [Entity Concept] -> Html ()
topic topic concepts = article_ $ topicDetailed topic concepts

topicSimple :: Entity Topic -> Html ()
topicSimple topic = do
    topicLink topic $ topicHeading topic
    topicText topic

topicDetailed :: Entity Topic -> [Entity Concept] -> Html ()
topicDetailed topic concepts = do
    topicLink topic $ topicHeading topic
    topicText topic
    
    topicConceptsHeading
    case concepts of
        [] -> topicConceptsMissing
        concepts -> unorderedList $ map conceptSimple concepts

topicUri topic = mappend "/topic/" (topicTitle $ entityVal topic)
topicLink topic html = link (topicUri topic) html

topicHeading = h1_ . toHtml . topicTitle . entityVal
topicText = p_ . toHtml . topicSummary . entityVal
topicConceptsHeading = h2_ "Concepts"
topicConceptsMissing = p_ "This topic has no concepts"
