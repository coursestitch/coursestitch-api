{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Handlers.Resource where

import Text.Read (readMaybe)
import Data.Int (Int64)
import Data.Text (strip, split, unpack)
import Data.String (fromString)

import Database.Persist (Entity, entityVal, toBackendKey)

import CourseStitch.Handlers.Utils
import CourseStitch.Handlers.User (authenticate)
import CourseStitch.Handlers.Resource
import CourseStitch.Handlers.Concept (withConceptId)
import CourseStitch.Models.RunDB

import qualified Templates

resourceNew :: RunDB -> ActionM ()
resourceNew runDB = do
    template $ Templates.page $ Templates.resourceForm Nothing

resourcePage :: RunDB -> ActionM ()
resourcePage runDB = authenticate runDB fail success where
    success user = withResourceId $ \id -> do
        result <- runDB $ getResourceWithConceptMastery user id
        case result of
            Nothing -> notFound404 "resource"
            Just (rs, cs) -> template $ Templates.resourceConceptsMastery rs cs
    fail = resourceAction runDB $ \_ resource concepts -> do
        template $ do
            Templates.resourcePage resource concepts
            Templates.resourceConcepts resource concepts

resourceEdit :: RunDB -> ActionM ()
resourceEdit runDB = resourceAction runDB $ \id resource concepts -> do
    topics <- runDB (getTopicsFromResource resource)
    relationships <- runDB (getRelationshipsFromResource resource)
    
    template $ Templates.page $ do
        Templates.resourceForm $ Just resource
        Templates.resourceRelationships resource topics relationships

resourceTopic :: RunDB -> ActionM ()
resourceTopic runDB = resourceAction runDB $ \id resource concepts -> do
    let resourceId = toSqlKey id
    relationships <- runDB (getRelationshipsFromResource resource)
    topicId <- param "topic"

    case readMaybe topicId of
        Nothing -> badRequest400 "Topic id should be an integer"
        Just topicId -> do
            topic <- runDB $ getTopic topicId
            case topic of
                Nothing -> notFound404 "Topic"
                Just (topic, concepts) -> do
                    template $ Templates.topicRelationships resourceId (map entityVal relationships) topic concepts

resourceRelationship :: RunDB -> ActionM ()
resourceRelationship runDB = resourceAction runDB $ \id resource concepts -> do
    let resourceId = toSqlKey id
    relationships <- runDB (getRelationshipsFromResource resource)
    
    withConceptId $ \conceptId -> do
        concept <- runDB $ getConcept conceptId
        case concept of
            Nothing -> notFound404 "concept"
            Just (concept, _) -> do
                template $ Templates.relationship resourceId (map entityVal relationships) concept
