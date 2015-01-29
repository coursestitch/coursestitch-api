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

resourceUpdate :: ConnectionPool -> ActionM ()
resourceUpdate pool = do
    id       <- param "resource"

    title    <- param "title"
    media    <- param "media"
    url      <- param "url"
    course   <- param "course"
    summary  <- param "summary"
    preview  <- param "preview"
    keywords <- param "keywords"

    let resource = Resource title media url course summary preview keywords

    case readMaybe id of
        Nothing -> badRequest400 "Resources should be of the form /resource/<integer>"
        Just id -> do
            liftIO $ runSqlPool (editResource id resource) pool
            resource <- liftIO $ runSqlPool (getResource id) pool
            case resource of
                Nothing                   -> notFound404 "resource"
                Just (resource, concepts) -> template $ Template.resourceUpdated resource concepts

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
