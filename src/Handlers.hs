{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import Text.Read (readMaybe)
import Data.String (fromString)
import Data.Monoid (mconcat)
import Control.Monad.IO.Class (liftIO)

import Network.HTTP.Types.Status (status404, status400)
import Web.Scotty (ActionM, text, param, status)

import Database.Persist.Sql (ConnectionPool, runSqlPool)

import Model
import Model.Queries

import qualified Template
import Template (template)

notFound404 entity = do
    status status404
    text $ mconcat ["No ", entity, " found"]

badRequest400 msg = do
    status status400
    text msg


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
        Nothing -> badRequest400 "Resources should be of the form /resource/<integer>"
        Just id -> do
            resource <- liftIO $ runSqlPool (getResource id) pool
            case resource of
                Nothing                   -> notFound404 "resource"
                Just (resource, concepts) -> template $ Template.resource resource concepts

resourceNew :: ConnectionPool -> ActionM ()
resourceNew pool = do
    template $ Template.resourceForm Nothing

resourceEdit :: ConnectionPool -> ActionM ()
resourceEdit pool = do
    id <- param "resource"
    case readMaybe id of
        Nothing -> badRequest400 "Resources should be of the form /resource/<integer>"
        Just id -> do
            resourceConcepts <- liftIO $ runSqlPool (getResource id) pool
            let resource = fmap fst resourceConcepts
            template $ Template.resourceForm resource


concepts :: ConnectionPool -> ActionM ()
concepts pool = do
    conceptList <- liftIO $ runSqlPool getConcepts pool
    template $ Template.concepts conceptList

concept :: ConnectionPool -> ActionM ()
concept pool = do
    title <- param "concept"
    concept <- liftIO $ runSqlPool (getConcept title) pool
    case concept of
        Nothing                   -> notFound404 "concept"
        Just (concept, resources) -> template $ Template.concept concept resources


topics :: ConnectionPool -> ActionM ()
topics pool = do
    topicList <- liftIO $ runSqlPool getTopics pool
    template $ Template.topics topicList

topic :: ConnectionPool -> ActionM ()
topic pool = do
    title <- param "topic"
    topic <- liftIO $ runSqlPool (getTopic title) pool
    case topic of Nothing                -> notFound404 "topic" 
                  Just (topic, concepts) -> template $ Template.topic topic concepts
