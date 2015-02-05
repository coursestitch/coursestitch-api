{-# LANGUAGE OverloadedStrings #-}

module Template.Relationship where

import Data.String (fromString)
import Data.Text.Encoding (decodeUtf8)
import Data.Monoid (mappend, mconcat)

import Network.HTTP.Types.Method (methodGet, methodPost, methodPut, methodDelete)

import Lucid
import Model
import Database.Persist (Entity, entityKey, entityVal)
import Database.Persist.Sql (unSqlBackendKey)

import Template.Template
import Template.Resource (resourceSimple)
import Template.Concept (conceptSimple)

relationships :: [Entity Relationship] -> Html ()
relationships cs = unorderedList $ map relationshipSimple cs

relationship :: Entity Relationship -> Entity Resource -> Entity Concept -> Html ()
relationship relationship resource concept = article_ $ relationshipDetailed relationship resource concept

relationshipCreated :: Entity Relationship -> Entity Resource -> Entity Concept -> Html ()
relationshipCreated rel r c = do
    p_ $ mconcat [relationshipUri rel, " was created successfully"]
    relationship rel r c

relationshipUpdated :: Entity Relationship -> Entity Resource -> Entity Concept -> Html ()
relationshipUpdated rel r c = do
    p_ $ mconcat [relationshipUri rel, " was updated successfully"]
    relationship rel r c

relationshipDeleted :: Entity Relationship -> Entity Resource -> Entity Concept -> Html ()
relationshipDeleted rel r c = do
    p_ $ mconcat [relationshipUri rel, " was deleted"]
    relationship rel r c

relationshipSimple :: Entity Relationship -> Html ()
relationshipSimple relationship = do
    relationshipLink relationship $ relationshipHeading relationship

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
                Just relationship -> relationshipUri relationship
                Nothing       -> "/relationship"
          method = case relationship of
                Just _  -> decodeUtf8 methodPut
                Nothing -> decodeUtf8 methodPost

relationshipDetailed :: Entity Relationship -> Entity Resource -> Entity Concept -> Html ()
relationshipDetailed relationship resource concept = do
    relationshipLink relationship $ do
        resourceSimple resource
        relationshipHeading relationship
        conceptSimple concept
    
relationshipUri relationship = mappend "/relationship/" ((fromString . show . unSqlBackendKey . unRelationshipKey . entityKey) relationship)
relationshipLink relationship html = link (relationshipUri relationship) html

relationshipHeading = h1_ . toHtml . show . relationshipRelationship . entityVal
