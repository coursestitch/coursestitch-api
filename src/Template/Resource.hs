{-# LANGUAGE OverloadedStrings #-}

module Template.Resource where

import Data.String (fromString)
import Data.Text.Encoding (decodeUtf8)
import Data.Monoid (mappend, mconcat)
import Control.Monad (when)

import Network.HTTP.Types.Method (methodGet, methodPost, methodPut, methodDelete)

import Lucid
import Model
import Database.Persist (Entity, entityVal)

import Template.Template
import Template.Concept (conceptSimple)

resources :: [Entity Resource] -> Html ()
resources cs = unorderedList $ map resourceSimple cs

resource :: Entity Resource -> [(RelationshipType, [Entity Concept])] -> Html ()
resource resource concepts = article_ $ resourceDetailed resource concepts

resourceCreated :: Entity Resource -> [(RelationshipType, [Entity Concept])] -> Html ()
resourceCreated r cs = do
    p_ $ mconcat [resourceUri r, " was created successfully"]
    resource r cs

resourceUpdated :: Entity Resource -> [(RelationshipType, [Entity Concept])] -> Html ()
resourceUpdated r cs = do
    p_ $ mconcat [resourceUri r, " was updated successfully"]
    resource r cs

resourceDeleted :: Entity Resource -> [(RelationshipType, [Entity Concept])] -> Html ()
resourceDeleted r cs = do
    p_ $ mconcat [resourceUri r, " was deleted"]
    resource r cs

resourceSimple :: Entity Resource -> Html ()
resourceSimple resource = do
    resourceLink resource $ resourceHeading resource

resourceForm :: Maybe (Entity Resource) -> Html ()
resourceForm resource = do
    form_ [action_ uri, method_ method] $ do
        fieldset_ $ do
            input "URL" "url" $ get resourceUrl
            input "Media" "media" $ get resourceMedia
            input "Title" "title" $ get resourceTitle
            input "Course" "course" $ get resourceCourse
            textInput "Summary" "summary" $ get resourceSummary
        fieldset_ $ do
            textInput "Preview" "preview" $ get resourcePreview
            input "Keywords" "keywords" $ get resourceKeywords
        input_ [type_ "submit"] 

    script_ [src_ "/js/form-methods.js"] ("" :: String)

    where get f = fmap (f . entityVal) resource
          uri = case resource of
                Just resource -> resourceUri resource
                Nothing       -> "/resource"
          method = case resource of
                Just _  -> decodeUtf8 methodPut
                Nothing -> decodeUtf8 methodPost

resourceDetailed :: Entity Resource -> [(RelationshipType, [Entity Concept])] -> Html ()
resourceDetailed resource rels = do
    resourceLink resource $ resourceHeading resource
    resourceText resource
    resourceExternalLink resource $ resourceQuote resource
    
resourceConcepts :: Bool -> Entity Resource -> [(RelationshipType, [Entity Concept])] -> Html ()
resourceConcepts loggedIn resource rels = mconcat $ map doRelationship rels where
    doRelationship (rel, concepts) = do
        resourceConceptsHeading rel
        case concepts of
            [] -> resourceConceptsMissing rel
            concepts -> unorderedList $ map conceptWithCheckbox concepts
    conceptWithCheckbox c = do
        when loggedIn $ input_ [type_ "checkbox"]
        conceptSimple c

resourceUri resource = mappend "/resource/" ((fromString . show . entityId) resource)
resourceLink resource html = link (resourceUri resource) html

resourceHeading = h1_ . toHtml . resourceTitle . entityVal
resourceText = p_ . toHtml . resourceSummary . entityVal
resourceQuote = blockquote_ . toHtml . resourcePreview . entityVal
resourceExternalLink resource = link ((resourceUrl . entityVal) resource)

resourceConceptsHeading rel = h2_ ("Concepts " `mappend` (fromString . show) rel)
resourceConceptsMissing rel = p_ ("There are no concepts " `mappend` (fromString . show) rel `mappend` " by this resource")
