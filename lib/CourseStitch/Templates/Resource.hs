{-# LANGUAGE OverloadedStrings, FlexibleInstances, NoMonomorphismRestriction #-}

module CourseStitch.Templates.Resource where

import CourseStitch.Templates.Utils
import CourseStitch.Templates.Concept (conceptSimple)
import CourseStitch.Templates.Relationship (relationshipUri)

instance ToHtml (Entity Resource) where
    toHtml = resourceSimple

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

resourceSimple :: Monad m => Entity Resource -> HtmlT m ()
resourceSimple resource = do
    resourceLink resource $ resourceHeading resource

resourceUri resource = mappend "/resource/" ((fromString . show . entityId) resource)
resourceLink resource = link (resourceUri resource)

resourceHeading = h1_ . toHtml . resourceTitle . entityVal
