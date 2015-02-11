{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module CourseStitch.Handlers.Concept where

import Text.Read (readMaybe)

import Database.Persist (Entity, selectFirst, entityVal)
import Database.Persist.Sql (toSqlKey)

import CourseStitch.Handlers.Handlers
import qualified CourseStitch.Templates as Templates
import CourseStitch.Models.RunDB

concepts :: RunDB -> ActionM ()
concepts runDB = do
    conceptList <- runDB getConcepts
    template $ Templates.concepts conceptList

conceptNew :: RunDB -> ActionM ()
conceptNew runDB = do
    template $ Templates.conceptForm Nothing

conceptCreate :: RunDB -> ActionM ()
conceptCreate runDB = do
    createdConcept <- conceptFromParams

    concept <- runDB (newConcept createdConcept)
    case concept of
        Nothing      -> conflict409 "A concept with this URL already exists"
        Just concept -> template $ Templates.conceptCreated concept

concept :: RunDB -> ActionM ()
concept runDB = conceptAction runDB $ \name concept topic resources -> do
    template $ Templates.concept concept

conceptEdit :: RunDB -> ActionM ()
conceptEdit runDB = conceptAction runDB $ \name concept topic resources -> do
    template $ Templates.conceptForm $ Just concept

conceptUpdate :: RunDB -> ActionM ()
conceptUpdate runDB = do
    updatedConcept <- conceptFromParams

    conceptAction runDB $ \name concept topic resources -> do
            runDB (editConcept name updatedConcept)
            concept' <- runDB (getConcept name)
            case concept' of
                Nothing                   -> notFound404 "concept"
                Just (concept, resources) -> template $ Templates.conceptUpdated concept

conceptDelete :: RunDB -> ActionM ()
conceptDelete runDB = do
    conceptAction runDB $ \name concept topic resources -> do
        runDB (deleteConcept name)
        template $ Templates.conceptDeleted concept

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
