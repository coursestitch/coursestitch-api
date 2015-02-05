{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Handlers.Resource where

import Text.Read (readMaybe)
import Data.Int (Int64)

import Database.Persist (Entity)

import Handlers.Handlers
import Handlers.User (isLoggedIn)
import qualified Template
import Model.RunDB

resources :: RunDB -> ActionM ()
resources runDB = do
    resourceList <- runDB getResources
    template $ Template.resources resourceList

resourceNew :: RunDB -> ActionM ()
resourceNew runDB = do
    template $ Template.resourceForm Nothing

resourceCreate :: RunDB -> ActionM ()
resourceCreate runDB = do
    createdResource <- resourceFromParams

    resource <- runDB (newResource createdResource)
    case resource of
        Nothing -> conflict409 "A resource with this URL already exists"
        Just resource -> template $ Template.resourceCreated resource []

resource :: RunDB -> ActionM ()
resource runDB = resourceAction runDB $ \id resource concepts -> do
    loggedIn <- isLoggedIn runDB
    template $ do
        Template.resource resource concepts
        Template.resourceConcepts loggedIn resource concepts

resourceEdit :: RunDB -> ActionM ()
resourceEdit runDB = resourceAction runDB $ \id resource concepts -> do
    template $ Template.resourceForm $ Just resource

resourceUpdate :: RunDB -> ActionM ()
resourceUpdate runDB = do
    updatedResource <- resourceFromParams

    resourceAction runDB $ \id resource concepts -> do
            runDB (editResource id updatedResource)
            resource' <- runDB (getResource id)
            case resource' of
                Nothing                   -> notFound404 "resource"
                Just (resource, concepts) -> template $ Template.resourceUpdated resource concepts

resourceDelete :: RunDB -> ActionM ()
resourceDelete runDB = do
    resourceAction runDB $ \id resource concepts -> do
        runDB (deleteResource id)
        template $ Template.resourceDeleted resource concepts

resourceAction :: RunDB -> (Int64 -> Entity Resource -> [(RelationshipType, [Entity Concept])] -> ActionM ()) -> ActionM ()
resourceAction runDB action = do
    id <- param "resource"
    case readMaybe id of
        Nothing -> badRequest400 "Resources should be of the form /resource/<integer>"
        Just id -> do
            resource <- runDB (getResource id)
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
