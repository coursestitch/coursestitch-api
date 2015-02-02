{-# LANGUAGE OverloadedStrings #-}

module Handlers.Concept where

import Text.Read (readMaybe)

import Database.Persist (Entity)
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
        Just concept -> template $ Template.conceptCreated concept []

concept :: ConnectionPool -> ActionM ()
concept pool = conceptAction pool $ \name concept concepts -> do
    template $ Template.concept concept concepts

conceptEdit :: ConnectionPool -> ActionM ()
conceptEdit pool = conceptAction pool $ \name concept concepts -> do
    template $ Template.conceptForm $ Just concept

conceptUpdate :: ConnectionPool -> ActionM ()
conceptUpdate pool = do
    updatedConcept <- conceptFromParams

    conceptAction pool $ \name concept concepts -> do
            liftIO $ runSqlPool (editConcept name updatedConcept) pool
            concept' <- liftIO $ runSqlPool (getConcept name) pool
            case concept' of
                Nothing                   -> notFound404 "concept"
                Just (concept, resources) -> template $ Template.conceptUpdated concept concepts

conceptDelete :: ConnectionPool -> ActionM ()
conceptDelete pool = do
    conceptAction pool $ \name concept concepts -> do
        liftIO $ runSqlPool (deleteConcept name) pool
        template $ Template.conceptDeleted concept concepts

conceptAction :: ConnectionPool -> (String -> Entity Concept -> [(RelationshipType, [Entity Resource])] -> ActionM ()) -> ActionM ()
conceptAction pool action = do
    name <- param "concept"
    concept <- liftIO $ runSqlPool (getConcept name) pool
    case concept of
        Nothing                   -> notFound404 "concept"
        Just (concept, resources) -> action name concept resources

conceptFromParams :: ActionM Concept
conceptFromParams = do
    title    <- param "title"
    topic    <- param "topic"

    return $ Concept (fmap toSqlKey $ readMaybe topic) title
