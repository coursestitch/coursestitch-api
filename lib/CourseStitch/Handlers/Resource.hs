{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module CourseStitch.Handlers.Resource where

import Data.Int (Int64)
import Data.Text (strip, split, unpack)
import Data.String (fromString)

import CourseStitch.Handlers.Utils
import CourseStitch.Handlers.User (authenticate)
import qualified CourseStitch.Templates as Templates

resources :: RunDB -> ActionM ()
resources runDB = do
    resourceList <- runDB getResources
    template $ Templates.resources resourceList

resourceCreate :: RunDB -> ActionM ()
resourceCreate runDB = do
    createdResource <- resourceFromParams

    resource <- runDB (newResource createdResource)
    case resource of
        Nothing -> conflict409 "A resource with this URL already exists"
        Just resource -> template $ Templates.resourceCreated resource

resource :: RunDB -> ActionM ()
resource runDB = resourceAction runDB $ \id resource concepts -> do
    content resource

resourceUpdate :: RunDB -> ActionM ()
resourceUpdate runDB = do
    updatedResource <- resourceFromParams

    resourceAction runDB $ \id resource concepts -> do
            runDB (editResource id updatedResource)
            resource' <- runDB (getResource id)
            case resource' of
                Nothing                   -> notFound404 "resource"
                Just (resource, concepts) -> template $ Templates.resourceUpdated resource

resourceDelete :: RunDB -> ActionM ()
resourceDelete runDB = do
    resourceAction runDB $ \id resource concepts -> do
        runDB (deleteResource id)
        template $ Templates.resourceDeleted resource

withResourceId :: (Int64 -> ActionM ()) -> ActionM ()
withResourceId action = do
    id <- param "resource"
    case readMaybe id of
        Nothing -> badRequest400 "Resources should be of the form /resource/<integer>"
        Just id -> action id

resourceAction :: RunDB -> (Int64 -> Entity Resource -> [(RelationshipType, [Entity Concept])] -> ActionM ()) -> ActionM ()
resourceAction runDB action = withResourceId $ \id -> do
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
