{-# LANGUAGE OverloadedStrings #-}

module CourseStitch.Templates.Topic where

import Data.Text.Encoding (decodeUtf8)
import Data.Monoid (mappend)

import Network.HTTP.Types.Method (methodGet, methodPost, methodPut, methodDelete)

import Lucid
import CourseStitch.Models
import Database.Persist (Entity, entityVal)

import CourseStitch.Templates.Templates
import CourseStitch.Templates.Concept

topics :: [Entity Topic] -> Html ()
topics ts = unorderedList $ map topicSimple ts

topic :: Entity Topic -> [Entity Concept] -> Html ()
topic topic concepts = article_ $ topicDetailed topic concepts

topicSimple :: Entity Topic -> Html ()
topicSimple topic = do
    topicLink topic $ topicHeading topic
    topicText topic

topicForm :: Maybe (Entity Topic) -> Html ()
topicForm topic = do
    form_ [action_ uri, method_ method] $ do
        fieldset_ $ do
            input "Title" "title" $ get topicTitle
            textInput "Summary" "summary" $ get topicSummary
        input_ [type_ "submit"]

    script_ [src_ "/js/form-methods.js"] ("" :: String)

    where get f = fmap (f . entityVal) topic
          uri = case topic of
                Just topic -> topicUri topic
                Nothing       -> "/topic"
          method = case topic of
                Just _  -> decodeUtf8 methodPut
                Nothing -> decodeUtf8 methodPost

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
