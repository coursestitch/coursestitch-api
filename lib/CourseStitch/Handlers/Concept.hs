{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module CourseStitch.Handlers.Concept where

import Data.Int (Int64)

import CourseStitch.Handlers.Utils
import qualified CourseStitch.Templates as Templates
import CourseStitch.Models.RunDB

concepts :: RunDB -> ActionM ()
concepts runDB = do
    conceptList <- runDB getConcepts
    content conceptList

conceptCreate :: RunDB -> ActionM ()
conceptCreate runDB = do
    createdConcept <- conceptFromParams

    concept <- runDB (newConcept createdConcept)
    case concept of
        Nothing      -> conflict409 "A concept with this URL already exists"
        Just concept -> content concept

concept :: RunDB -> ActionM ()
concept runDB = conceptAction runDB $ \id concept topic resources -> do
    content concept

conceptUpdate :: RunDB -> ActionM ()
conceptUpdate runDB = do
    updatedConcept <- conceptFromParams

    conceptAction runDB $ \id concept topic resources -> do
            runDB (editConcept id updatedConcept)
            concept' <- runDB (getConcept id)
            case concept' of
                Nothing                   -> notFound404 "concept"
                Just (concept, resources) -> content concept

conceptDelete :: RunDB -> ActionM ()
conceptDelete runDB = do
    conceptAction runDB $ \id concept topic resources -> do
        runDB (deleteConcept id)
        content concept

withConceptId :: (Int64 -> ActionM ()) -> ActionM ()
withConceptId action = do
    id <- param "concept"
    case readMaybe id of
        Nothing -> badRequest400 "Concepts should be of the form /concept/<integer>"
        Just id -> action id

conceptAction :: RunDB
                 -> (Int64 -> Entity Concept -> Maybe (Entity Topic) -> [(RelationshipType, [Entity Resource])] -> ActionM ())
                 -> ActionM ()
conceptAction runDB action = do
    withConceptId $ \id -> do
        concept <- runDB (getConcept id)
        case concept of
            Nothing                   -> notFound404 "concept"
            Just (concept, resources) -> do
                topic <- runDB (getConceptTopic concept)
                action id concept topic resources

conceptFromParams :: ActionM Concept
conceptFromParams = do
    title    <- param "title"
    topic    <- param "topic"

    return $ Concept (fmap toSqlKey $ readMaybe topic) title
