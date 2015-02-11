{-# LANGUAGE OverloadedStrings #-}

module CourseStitch.Templates.Relationship where

import Database.Persist (toBackendKey)
import Database.Persist.Sql (unSqlBackendKey)

import CourseStitch.Templates.Utils
import {-# SOURCE #-} CourseStitch.Templates.Resource (resourceSimple)
import CourseStitch.Templates.Concept (conceptSimple)

relationships :: [Entity Relationship] -> Html ()
relationships cs = unorderedList $ map relationshipSimple cs

relationship :: Entity Relationship -> Html ()
relationship relationship = article_ $ relationshipSimple relationship

relationshipCreated :: Entity Relationship -> Html ()
relationshipCreated rel = do
    p_ $ mconcat [relationshipUri (entityVal rel), " was created successfully"]
    relationship rel

relationshipUpdated :: Entity Relationship -> Html ()
relationshipUpdated rel = do
    p_ $ mconcat [relationshipUri (entityVal rel), " was updated successfully"]
    relationship rel

relationshipDeleted :: Entity Relationship -> Html ()
relationshipDeleted rel = do
    p_ $ mconcat [relationshipUri (entityVal rel), " was deleted"]
    relationship rel

relationshipSimple :: Entity Relationship -> Html ()
relationshipSimple relationship = do
    relationshipLink (entityVal relationship) $ relationshipHeading relationship
    
relationshipUri relationship = mconcat ["/relationship",
    "/resource/", resourceKey $ get relationshipResource,
    "/", fromString . show $ get relationshipRelationship,
    "/concept/", conceptKey $ get relationshipConcept
    ]
    where get f = f relationship
          conceptKey  = (fromString . show . unSqlBackendKey . toBackendKey)
          resourceKey = (fromString . show . unSqlBackendKey . toBackendKey)
relationshipLink relationship html = link (relationshipUri relationship) html

relationshipHeading = h1_ . toHtml . show . relationshipRelationship . entityVal
