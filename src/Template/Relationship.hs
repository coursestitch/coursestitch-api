{-# LANGUAGE OverloadedStrings #-}

module Template.Relationship where

import Data.String (fromString)
import Data.Text.Encoding (decodeUtf8)
import Data.Monoid (mappend, mconcat)

import Network.HTTP.Types.Method (methodGet, methodPost, methodPut, methodDelete)

import Lucid
import Model
import Database.Persist (Entity, entityKey, entityVal, toBackendKey)
import Database.Persist.Sql (unSqlBackendKey)

import Template.Template
import {-# SOURCE #-} Template.Resource (resourceSimple)
import Template.Concept (conceptSimple)

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

relationshipForm :: Maybe (Entity Relationship) -> Html ()
relationshipForm relationship = do
    form_ [action_ uri, method_ method] $ do
        fieldset_ $ do
            input "Resource" "resource" $ getResourceId relationshipResource
            input "Relationship Type" "relationship" $ get relationshipRelationship
            input "Concept" "concept" $ getConceptId relationshipConcept
        input_ [type_ "submit"] 

    script_ [src_ "/js/form-methods.js"] ("" :: String)

    where get f = fmap (fromString . show . f . entityVal) relationship
          getResourceId f = get (unSqlBackendKey . unResourceKey . f)
          getConceptId f = get (unSqlBackendKey . unConceptKey . f)
          uri = case relationship of
                Just relationship -> relationshipUri (entityVal relationship)
                Nothing       -> "/relationship"
          method = case relationship of
                Just _  -> decodeUtf8 methodPut
                Nothing -> decodeUtf8 methodPost

relationshipDetailed :: Entity Relationship -> Entity Resource -> Entity Concept -> Html ()
relationshipDetailed relationship resource concept = do
    relationshipLink (entityVal relationship) $ do
        resourceSimple resource
        relationshipHeading relationship
        conceptSimple concept
    
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
