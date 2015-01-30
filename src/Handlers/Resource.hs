{-# LANGUAGE OverloadedStrings #-}

module Handlers.Resource where

import Text.Read (readMaybe)
import Data.Int (Int64)

import Database.Persist (Entity)

import Handlers.Handlers
import qualified Template

resources :: ConnectionPool -> ActionM ()
resources pool = do
    resourceList <- liftIO $ runSqlPool getResources pool
    template $ Template.resources resourceList

resourceNew :: ConnectionPool -> ActionM ()
resourceNew pool = do
    template $ Template.resourceForm Nothing

resourceCreate :: ConnectionPool -> ActionM ()
resourceCreate pool = do
    createdResource <- resourceFromParams

    resource <- liftIO $ runSqlPool (newResource createdResource) pool
    template $ Template.resourceCreated resource []

resource :: ConnectionPool -> ActionM ()
resource pool = resourceAction pool $ \id resource concepts -> do
    template $ Template.resource resource concepts

resourceEdit :: ConnectionPool -> ActionM ()
resourceEdit pool = resourceAction pool $ \id resource concepts -> do
    template $ Template.resourceForm $ Just resource

resourceUpdate :: ConnectionPool -> ActionM ()
resourceUpdate pool = do
    updatedResource <- resourceFromParams

    resourceAction pool $ \id resource concepts -> do
            liftIO $ runSqlPool (editResource id updatedResource) pool
            resource' <- liftIO $ runSqlPool (getResource id) pool
            case resource' of
                Nothing                   -> notFound404 "resource"
                Just (resource, concepts) -> template $ Template.resourceUpdated resource concepts

resourceDelete :: ConnectionPool -> ActionM ()
resourceDelete pool = do
    resourceAction pool $ \id resource concepts -> do
        liftIO $ runSqlPool (deleteResource id) pool
        template $ Template.resourceDeleted resource concepts

resourceAction :: ConnectionPool -> (Int64 -> Entity Resource -> [(RelationshipType, [Entity Concept])] -> ActionM ()) -> ActionM ()
resourceAction pool action = do
    id <- param "resource"
    case readMaybe id of
        Nothing -> badRequest400 "Resources should be of the form /resource/<integer>"
        Just id -> do
            resource <- liftIO $ runSqlPool (getResource id) pool
            case resource of
                Nothing                   -> notFound404 "resource"
                Just (resource, concepts) -> action id resource concepts

resourceFromParams :: ActionM Resource
resourceFromParams = do
    title    <- param "title"
    media    <- param "media"
    url      <- param "url"
    course   <- param "course"
    summary  <- param "summary"
    preview  <- param "preview"
    keywords <- param "keywords"

    return $ Resource title media url course summary preview keywords
