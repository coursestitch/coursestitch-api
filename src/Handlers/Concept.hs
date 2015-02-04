{-# LANGUAGE OverloadedStrings #-}

module Handlers.Concept where

import Text.Read (readMaybe)

import Database.Persist (Entity, selectFirst, entityVal)
import Database.Persist.Sql (toSqlKey)

import Handlers.Handlers
import qualified Template

concepts :: ConnectionPool -> ActionM ()
concepts pool = do
    conceptList <- liftIO $ runSqlPool getConcepts pool
    template $ Template.concepts conceptList

conceptNew :: ConnectionPool -> ActionM ()
conceptNew pool = do
    template $ Template.conceptForm Nothing

conceptCreate :: ConnectionPool -> ActionM ()
conceptCreate pool = do
    createdConcept <- conceptFromParams

    concept <- liftIO $ runSqlPool (newConcept createdConcept) pool
    case concept of
        Nothing      -> conflict409 "A concept with this URL already exists"
        Just concept -> template $ Template.conceptCreated concept Nothing []

concept :: ConnectionPool -> ActionM ()
concept pool = conceptAction pool $ \name concept topic resources -> do
    template $ Template.concept concept topic resources

conceptPage :: ConnectionPool -> ActionM ()
conceptPage pool = conceptAction pool $ \name concept topic resources -> do
    template $ Template.conceptPage concept topic resources

conceptEdit :: ConnectionPool -> ActionM ()
conceptEdit pool = conceptAction pool $ \name concept topic resources -> do
    template $ Template.conceptForm $ Just concept

conceptUpdate :: ConnectionPool -> ActionM ()
conceptUpdate pool = do
    updatedConcept <- conceptFromParams

    conceptAction pool $ \name concept topic resourcess -> do
            liftIO $ runSqlPool (editConcept name updatedConcept) pool
            concept' <- liftIO $ runSqlPool (getConcept name) pool
            case concept' of
                Nothing                   -> notFound404 "concept"
                Just (concept, resources) -> template $ Template.conceptUpdated concept topic resources

conceptDelete :: ConnectionPool -> ActionM ()
conceptDelete pool = do
    conceptAction pool $ \name concept topic resources -> do
        liftIO $ runSqlPool (deleteConcept name) pool
        template $ Template.conceptDeleted concept topic resources

conceptAction :: ConnectionPool
                 -> (String -> Entity Concept -> Maybe (Entity Topic) -> [(RelationshipType, [Entity Resource])] -> ActionM ())
                 -> ActionM ()
conceptAction pool action = do
    name <- param "concept"
    concept <- liftIO $ runSqlPool (getConcept name) pool
    case concept of
        Nothing                   -> notFound404 "concept"
        Just (concept, resources) -> do
            topic <- liftIO $ runSqlPool (getConceptTopic concept) pool
            action name concept topic resources

conceptFromParams :: ActionM Concept
conceptFromParams = do
    title    <- param "title"
    topic    <- param "topic"

    return $ Concept (fmap toSqlKey $ readMaybe topic) title
