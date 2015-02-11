{-# LANGUAGE OverloadedStrings #-}

module CourseStitch.Templates.Resource where

import Data.String (fromString)
import Data.Text.Encoding (decodeUtf8)
import Data.Monoid (mappend, (<>), mconcat)
import Control.Monad (when, liftM3)

import Network.HTTP.Types.Method (methodGet, methodPost, methodPut, methodDelete)

import Lucid
import Database.Persist (Entity(..), entityKey, entityVal, toBackendKey)
import Database.Persist.Sql (unSqlBackendKey)

import CourseStitch.Models
import CourseStitch.Models.Models (entityId)
import CourseStitch.Templates.Utils
import CourseStitch.Templates.Concept (conceptSimple)
import CourseStitch.Templates.Relationship (relationshipUri)

resources :: [Entity Resource] -> Html ()
resources cs = unorderedList $ map resourceSimple cs

resource :: Entity Resource -> Html ()
resource resource = article_ $ resourceSimple resource

resourceCreated :: Entity Resource -> Html ()
resourceCreated r = do
    p_ $ mconcat [resourceUri r, " was created successfully"]
    resource r

resourceUpdated :: Entity Resource -> Html ()
resourceUpdated r = do
    p_ $ mconcat [resourceUri r, " was updated successfully"]
    resource r

resourceDeleted :: Entity Resource -> Html ()
resourceDeleted r = do
    p_ $ mconcat [resourceUri r, " was deleted"]
    resource r

resourceSimple :: Entity Resource -> Html ()
resourceSimple resource = do
    resourceLink resource $ resourceHeading resource

resourceUri resource = mappend "/resource/" ((fromString . show . entityId) resource)
resourceLink resource html = link (resourceUri resource) html

resourceHeading = h1_ . toHtml . resourceTitle . entityVal
