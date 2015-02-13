{-# LANGUAGE OverloadedStrings #-}

module Templates.Resource where

import Data.String (fromString)
import Data.Text.Encoding (decodeUtf8)
import Data.Monoid (mappend, (<>), mconcat)
import Control.Monad (when, liftM3)

import Network.HTTP.Types.Method (methodGet, methodPost, methodPut, methodDelete)

import Lucid
import CourseStitch.Models
import Database.Persist (Entity(..), entityKey, entityVal, toBackendKey)
import Database.Persist.Sql (unSqlBackendKey)

import CourseStitch.Templates.Utils
import CourseStitch.Templates.Resource
import CourseStitch.Templates.Concept (conceptSimple)
import CourseStitch.Templates.Relationship (relationshipUri)
import Templates.Website (page , typeahead)

resourceDetailed :: Entity Resource -> [(RelationshipType, [Entity Concept])] -> Html ()
resourceDetailed resource rels = do
    resourceLink resource $ resourceHeading resource
    resourceText resource
    resourceExternalLink resource $ resourceQuote resource

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

resourceRelationships :: Entity Resource -> [(Entity Topic, [Entity Concept])] -> [Entity Relationship] -> Html ()
resourceRelationships resource topics relationships = do
    script_ [src_ "/js/request.js"] ("" :: String)
    script_ [src_ "/js/checkbox-change.js"] ("" :: String)
    script_ [src_ "/js/add-topic.js"] ("" :: String)
    ul_ [class_ "topic-list"] $
        mconcat $ (map li_) $ map (uncurry (topicRelationships resource relationships)) topics
    typeahead "/topic" "Enter a new topic" ["add-topic"]

topicRelationships :: Entity Resource -> [Entity Relationship] -> Entity Topic -> [Entity Concept] -> Html ()
topicRelationships resource relationships topic concepts = do
    h1_ $ (toHtml . topicTitle . entityVal) topic
    unorderedList $ map (relationship resource relationships) concepts

relationship :: Entity Resource -> [Entity Relationship] -> Entity Concept -> Html ()
relationship resource relationships concept = do
    (toHtml . conceptTitle . entityVal) concept
    br_ []
    mconcat $ map checkbox [Taught ..]
    
    where checkbox rel = do
            input_ ([id_ $ id rel, type_ "checkbox", name_ "relationship", onchange_ $ update rel] ++ checked rel)
            label_ [for_ $ id rel] $ (toHtml . show) rel
          checked rel = if elem (relationship rel) (map entityVal relationships) then [checked_] else []
          id rel = mconcat [(fromString . show) rel, key concept]
          key = (fromString . show . unSqlBackendKey . toBackendKey . entityKey)
          relationship rel = Relationship (entityKey resource) rel (entityKey concept)
          update rel = fromString $ concat ["checkboxChange(this,",
            "request.bind(this, 'PUT', '"++uri (relationship rel)++"'),",
            "request.bind(this, 'DELETE', '"++uri (relationship rel)++"')",
            ")"]
          uri = relationshipUri
    
resourceConcepts :: Entity Resource -> [(RelationshipType, [Entity Concept])] -> Html ()
resourceConcepts resource rels = mconcat $ map doRelationship rels where
    doRelationship (rel, concepts) = do
        resourceConceptsHeading rel
        case concepts of
            [] -> resourceConceptsMissing rel
            concepts -> unorderedList $ map conceptSimple concepts

resourceConceptsMastery :: Entity Resource -> [(RelationshipType, [(Entity Concept, Bool)])] -> Html ()
resourceConceptsMastery resource rels = mconcat $ map doRelationship rels where
    doRelationship (rel, concepts) = do
        resourceConceptsHeading rel
        case concepts of
            [] -> resourceConceptsMissing rel
            concepts -> unorderedList $ map conceptWithCheckbox concepts
    conceptWithCheckbox (c, m) = do
        input_ ([type_ "checkbox"] <> if m then [checked_] else [])
        conceptSimple c

resourceText = p_ . toHtml . resourceSummary . entityVal
resourceExternalLink resource = link ((resourceUrl . entityVal) resource)
resourceQuote = blockquote_ . toHtml . resourcePreview . entityVal
resourceConceptsHeading rel = h2_ ("Concepts " `mappend` (fromString . show) rel)
resourceConceptsMissing rel = p_ ("There are no concepts " `mappend` (fromString . show) rel `mappend` " by this resource")

resourcePage :: Entity Resource -> [(RelationshipType, [Entity Concept])] -> Html()
resourcePage r rels = page $ resourceDetailed r rels
