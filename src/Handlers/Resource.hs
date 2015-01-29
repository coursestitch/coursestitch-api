{-# LANGUAGE OverloadedStrings #-}

module Handlers.Resource where

import Handlers.Handlers
import qualified Template

import Text.Read (readMaybe)

resources :: ConnectionPool -> ActionM ()
resources pool = do
    resourceList <- liftIO $ runSqlPool getResources pool
    template $ Template.resources resourceList

resource :: ConnectionPool -> ActionM ()
resource pool = do
    id <- param "resource"
    case readMaybe id of
        Nothing -> badRequest400 "Resources should be of the form /resource/<integer>"
        Just id -> do
            resource <- liftIO $ runSqlPool (getResource id) pool
            case resource of
                Nothing                   -> notFound404 "resource"
                Just (resource, concepts) -> template $ Template.resource resource concepts

resourceNew :: ConnectionPool -> ActionM ()
resourceNew pool = do
    template $ Template.resourceForm Nothing

resourceEdit :: ConnectionPool -> ActionM ()
resourceEdit pool = do
    id <- param "resource"
    case readMaybe id of
        Nothing -> badRequest400 "Resources should be of the form /resource/<integer>"
        Just id -> do
            resourceConcepts <- liftIO $ runSqlPool (getResource id) pool
            let resource = fmap fst resourceConcepts
            template $ Template.resourceForm resource
