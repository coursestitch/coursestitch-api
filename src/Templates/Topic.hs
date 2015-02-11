{-# LANGUAGE OverloadedStrings #-}

module Templates.Topic where

import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types.Method (methodGet, methodPost, methodPut, methodDelete)

import CourseStitch.Templates.Topic
import CourseStitch.Templates.Utils
import CourseStitch.Templates.Concept

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

topicConceptsHeading = h2_ "Concepts"
topicConceptsMissing = p_ "This topic has no concepts"
