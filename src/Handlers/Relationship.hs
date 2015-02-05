{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Handlers.Relationship where

import Data.Int (Int64)
import Text.Read (readMaybe)

import Database.Persist (Entity, selectFirst, entityKey, entityVal)
import Database.Persist.Sql (toSqlKey, unSqlBackendKey)

import Handlers.Handlers
import qualified Template
import Model.RunDB

relationships :: RunDB -> ActionM ()
relationships runDB = do
    relationshipList <- runDB getRelationships
    template $ Template.relationships relationshipList

relationshipNew :: RunDB -> ActionM ()
relationshipNew runDB = do
    template $ Template.relationshipForm Nothing

relationshipCreate :: RunDB -> ActionM ()
relationshipCreate runDB = do
    createdRelationship <- relationshipFromParams

    case createdRelationship of
        Nothing -> badRequest400 "Could not read the relationship from the request"
        Just createdRelationship -> do
            relationship  <- runDB (newRelationship createdRelationship)
            case relationship of
                Nothing           -> conflict409 "A relationship with this URL already exists"
                Just relationship -> do
                    let id = (unSqlBackendKey . unRelationshipKey . entityKey) relationship
                    relationship' <- runDB (getRelationship id)
                    case relationship' of
                        Nothing                       -> notFound404 "relationship"
                        Just (rel, resource, concept) -> template $ Template.relationshipCreated relationship resource concept

relationship :: RunDB -> ActionM ()
relationship runDB = relationshipAction runDB $ \id relationship resource concept -> do
    template $ Template.relationship relationship resource concept

relationshipEdit :: RunDB -> ActionM ()
relationshipEdit runDB = relationshipAction runDB $ \id relationship resource concept -> do
    template $ Template.relationshipForm $ Just relationship

relationshipUpdate :: RunDB -> ActionM ()
relationshipUpdate runDB = do
    updatedRelationship <- relationshipFromParams

    case updatedRelationship of
        Nothing -> badRequest400 "Could not read the relationship from the request"
        Just updatedRelationship -> do
            relationshipAction runDB $ \id relationship resource concept -> do
                    runDB (editRelationship id updatedRelationship)
                    relationship' <- runDB (getRelationship id)
                    case relationship' of
                        Nothing                                -> notFound404 "relationship"
                        Just (relationship, resource, concept) -> template $ Template.relationshipUpdated relationship resource concept

relationshipDelete :: RunDB -> ActionM ()
relationshipDelete runDB = do
    relationshipAction runDB $ \id relationship resource concept -> do
        runDB (deleteRelationship id)
        template $ Template.relationshipDeleted relationship resource concept

relationshipAction :: RunDB
                 -> (Int64 -> Entity Relationship -> Entity Resource -> Entity Concept -> ActionM ())
                 -> ActionM ()
relationshipAction runDB action = do
    id <- param "relationship"
    case readMaybe id of
        Nothing -> badRequest400 "Relationships should be of the form /relationship/<integer>"
        Just id -> do
            resource <- runDB (getRelationship id)
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
