{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Handlers.Concept where

import Text.Read (readMaybe)

import Database.Persist (Entity, selectFirst, entityVal)
import Database.Persist.Sql (toSqlKey)

import Handlers.Handlers
import qualified Template
import Model.RunDB

concepts :: RunDB -> ActionM ()
concepts runDB = do
    conceptList <- runDB getConcepts
    template $ Template.concepts conceptList

conceptNew :: RunDB -> ActionM ()
conceptNew runDB = do
    template $ Template.conceptForm Nothing

conceptCreate :: RunDB -> ActionM ()
conceptCreate runDB = do
    createdConcept <- conceptFromParams

    concept <- runDB (newConcept createdConcept)
    case concept of
        Nothing      -> conflict409 "A concept with this URL already exists"
        Just concept -> template $ Template.conceptCreated concept Nothing []

concept :: RunDB -> ActionM ()
concept runDB = conceptAction runDB $ \name concept topic resources -> do
    template $ Template.concept concept topic resources

conceptEdit :: RunDB -> ActionM ()
conceptEdit runDB = conceptAction runDB $ \name concept topic resources -> do
    template $ Template.conceptForm $ Just concept

conceptUpdate :: RunDB -> ActionM ()
conceptUpdate runDB = do
    updatedConcept <- conceptFromParams

    conceptAction runDB $ \name concept topic resources -> do
            runDB (editConcept name updatedConcept)
            concept' <- runDB (getConcept name)
            case concept' of
                Nothing                   -> notFound404 "concept"
                Just (concept, resources) -> template $ Template.conceptUpdated concept topic resources

conceptDelete :: RunDB -> ActionM ()
conceptDelete runDB = do
    conceptAction runDB $ \name concept topic resources -> do
        runDB (deleteConcept name)
        template $ Template.conceptDeleted concept topic resources

conceptAction :: RunDB
                 -> (String -> Entity Concept -> Maybe (Entity Topic) -> [(RelationshipType, [Entity Resource])] -> ActionM ())
                 -> ActionM ()
conceptAction runDB action = do
    name <- param "concept"
    concept <- runDB (getConcept name)
    case concept of
        Nothing                   -> notFound404 "concept"
        Just (concept, resources) -> do
            topic <- runDB (getConceptTopic concept)
            action name concept topic resources

conceptFromParams :: ActionM Concept
conceptFromParams = do
    title    <- param "title"
    topic    <- param "topic"

    return $ Concept (fmap toSqlKey $ readMaybe topic) title
