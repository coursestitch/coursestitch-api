{-# LANGUAGE OverloadedStrings #-}

module Handlers.Relationship where

import Data.Int (Int64)
import Text.Read (readMaybe)

import Database.Persist (Entity, selectFirst, entityKey, entityVal)
import Database.Persist.Sql (toSqlKey, unSqlBackendKey)

import Handlers.Handlers
import qualified Template

relationships :: ConnectionPool -> ActionM ()
relationships pool = do
    relationshipList <- liftIO $ runSqlPool getRelationships pool
    template $ Template.relationships relationshipList

relationshipNew :: ConnectionPool -> ActionM ()
relationshipNew pool = do
    template $ Template.relationshipForm Nothing

relationshipCreate :: ConnectionPool -> ActionM ()
relationshipCreate pool = do
    createdRelationship <- relationshipFromParams

    case createdRelationship of
        Nothing -> badRequest400 "Could not read the relationship from the request"
        Just createdRelationship -> do
            relationship  <- liftIO $ runSqlPool (newRelationship createdRelationship) pool
            case relationship of
                Nothing           -> conflict409 "A relationship with this URL already exists"
                Just relationship -> do
                    let id = (unSqlBackendKey . unRelationshipKey . entityKey) relationship
                    relationship' <- liftIO $ runSqlPool (getRelationship id) pool
                    case relationship' of
                        Nothing                       -> notFound404 "relationship"
                        Just (rel, resource, concept) -> template $ Template.relationshipCreated relationship resource concept

relationship :: ConnectionPool -> ActionM ()
relationship pool = relationshipAction pool $ \id relationship resource concept -> do
    template $ Template.relationship relationship resource concept

relationshipEdit :: ConnectionPool -> ActionM ()
relationshipEdit pool = relationshipAction pool $ \id relationship resource concept -> do
    template $ Template.relationshipForm $ Just relationship

relationshipUpdate :: ConnectionPool -> ActionM ()
relationshipUpdate pool = do
    updatedRelationship <- relationshipFromParams

    case updatedRelationship of
        Nothing -> badRequest400 "Could not read the relationship from the request"
        Just updatedRelationship -> do
            relationshipAction pool $ \id relationship resource concept -> do
                    liftIO $ runSqlPool (editRelationship id updatedRelationship) pool
                    relationship' <- liftIO $ runSqlPool (getRelationship id) pool
                    case relationship' of
                        Nothing                                -> notFound404 "relationship"
                        Just (relationship, resource, concept) -> template $ Template.relationshipUpdated relationship resource concept

relationshipDelete :: ConnectionPool -> ActionM ()
relationshipDelete pool = do
    relationshipAction pool $ \id relationship resource concept -> do
        liftIO $ runSqlPool (deleteRelationship id) pool
        template $ Template.relationshipDeleted relationship resource concept

relationshipAction :: ConnectionPool
                 -> (Int64 -> Entity Relationship -> Entity Resource -> Entity Concept -> ActionM ())
                 -> ActionM ()
relationshipAction pool action = do
    id <- param "relationship"
    case readMaybe id of
        Nothing -> badRequest400 "Relationships should be of the form /relationship/<integer>"
        Just id -> do
            resource <- liftIO $ runSqlPool (getRelationship id) pool
            case resource of
                Nothing                                -> notFound404 "relationship"
                Just (relationship, resource, concept) -> action id relationship resource concept

relationshipFromParams :: ActionM (Maybe Relationship)
relationshipFromParams = do
    resource     <- param "resource"
    relationship <- param "relationship"
    concept      <- param "concept"

    return $ case (readMaybe resource, readMaybe relationship, readMaybe concept) of
        (Just resource, Just relationship, Just concept) -> Just $ Relationship (toSqlKey resource) relationship (toSqlKey concept)
        otherwise                     -> Nothing
