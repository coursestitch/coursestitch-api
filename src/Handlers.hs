{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import Text.Read (readMaybe)
import Control.Monad.IO.Class (liftIO)
import Data.String (fromString)
import Web.Scotty (ActionM, text, param)

import Database.Persist.Sql (ConnectionPool, runSqlPool)

import Model
import Model.Queries

import qualified Template
import Template (template)

root :: ConnectionPool -> ActionM ()
root pool = do
    topics <- liftIO $ runSqlPool getTopics pool
    text (fromString . show $ topics)


resources :: ConnectionPool -> ActionM ()
resources pool = do
    resourceList <- liftIO $ runSqlPool getResources pool
    template $ Template.resources resourceList

resource :: ConnectionPool -> ActionM ()
resource pool = do
    id <- param "resource"
    case readMaybe id of
        Nothing -> text "Resources should be of the form /resource/<integer>"
        Just id -> do
            resource <- liftIO $ runSqlPool (getResource id) pool
            case resource of
                Nothing       -> text "No resource found"
                Just resource -> template $ Template.resource resource


concepts :: ConnectionPool -> ActionM ()
concepts pool = do
    conceptList <- liftIO $ runSqlPool getConcepts pool
    template $ Template.concepts conceptList

concept :: ConnectionPool -> ActionM ()
concept pool = do
    title <- param "concept"
    concept <- liftIO $ runSqlPool (getConcept title) pool
    case concept of
        Nothing      -> text "No concept found"
        Just concept -> template $ Template.concept concept


topics :: ConnectionPool -> ActionM ()
topics pool = do
    topicList <- liftIO $ runSqlPool getTopics pool
    template $ Template.topics topicList

topic :: ConnectionPool -> ActionM ()
topic pool = do
    title <- param "topic"
    topic <- liftIO $ runSqlPool (getTopic title) pool
    case topic of Nothing                -> text "No topic found"
                  Just (topic, concepts) -> template $ Template.topic topic concepts
