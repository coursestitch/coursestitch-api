{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module CourseStitch.Handlers.Relationship where

import Data.Int (Int64)
import Text.Read (readMaybe)

import Database.Persist (Entity, selectFirst, entityKey, entityVal)
import Database.Persist.Sql (toSqlKey, unSqlBackendKey)

import CourseStitch.Handlers.Handlers
import qualified CourseStitch.Templates as Templates
import CourseStitch.Models.RunDB

relationships :: RunDB -> ActionM ()
relationships runDB = do
    relationshipList <- runDB getRelationships
    template $ Templates.relationships relationshipList

relationshipCreate :: RunDB -> ActionM ()
relationshipCreate runDB = do
    createdRelationship <- relationshipFromParams

    case createdRelationship of
        Nothing -> badRequest400 "Relationships should be of the form /relationship/resource/<integer>/<relationship type>/concept/<integer>"
        Just createdRelationship -> do
            relationship <- runDB (newRelationship createdRelationship)
            relationship' <- runDB (getRelationship $ entityVal relationship)
            case relationship' of
                Nothing                       -> notFound404 "relationship"
                Just (rel, resource, concept) -> template $ Templates.relationshipCreated relationship

relationship :: RunDB -> ActionM ()
relationship runDB = relationshipAction runDB $ \relationship resource concept -> do
    template $ Templates.relationship relationship

relationshipEdit :: RunDB -> ActionM ()
relationshipEdit runDB = relationshipAction runDB $ \relationship resource concept -> do
    template $ Templates.relationshipForm $ Just relationship

relationshipDelete :: RunDB -> ActionM ()
relationshipDelete runDB = do
    relationshipAction runDB $ \relationship resource concept -> do
        runDB (deleteRelationship relationship)
        template $ Templates.relationshipDeleted relationship

relationshipAction :: RunDB
                 -> (Entity Relationship -> Entity Resource -> Entity Concept -> ActionM ())
                 -> ActionM ()
relationshipAction runDB action = do
    relationship <- relationshipFromParams
    case relationship of
        Nothing -> badRequest400 "Relationships should be of the form /relationship/resource/<integer>/<relationship type>/concept/<integer>"
        Just relationship -> do
            relationship <- runDB (getRelationship relationship)
            case relationship of
                Nothing                                -> notFound404 "relationship"
                Just (relationship, resource, concept) -> action relationship resource concept

relationshipFromParams :: ActionM (Maybe Relationship)
relationshipFromParams = do
    resource     <- param "resource"
    relationship <- param "relationship"
    concept      <- param "concept"

    return $ case (readMaybe resource, readMaybe relationship, readMaybe concept) of
        (Just resource, Just relationship, Just concept) -> Just $ Relationship (toSqlKey resource) relationship (toSqlKey concept)
        otherwise                     -> Nothing
