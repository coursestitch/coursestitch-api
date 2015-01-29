{-# LANGUAGE OverloadedStrings #-}

module Handlers.Concept where

import Handlers.Handlers
import qualified Template

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
