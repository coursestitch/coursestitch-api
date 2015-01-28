{-# LANGUAGE OverloadedStrings #-}

module Template.Resource where

import Data.String (fromString)
import Data.Monoid (mappend, mconcat)

import Lucid
import Model
import Database.Persist (Entity, entityVal)

import Template.Template
import Template.Concept (conceptSimple)

resources :: [Entity Resource] -> Html ()
resources cs = unorderedList $ map resourceSimple cs

resource :: Entity Resource -> [(RelationshipType, [Entity Concept])] -> Html ()
resource resource concepts = article_ $ resourceDetailed resource concepts

resourceSimple :: Entity Resource -> Html ()
resourceSimple resource = do
    resourceLink resource $ resourceHeading resource

resourceDetailed :: Entity Resource -> [(RelationshipType, [Entity Concept])] -> Html ()
resourceDetailed resource rels = do
    resourceLink resource $ resourceHeading resource
    mconcat $ map (uncurry resourceConcepts) rels
    
resourceConcepts :: RelationshipType -> [Entity Concept] -> Html ()
resourceConcepts rel concepts = do
    resourceConceptsHeading rel
    case concepts of
        [] -> resourceConceptsMissing rel
        concepts -> unorderedList $ map conceptSimple concepts


resourceUri resource = mappend "/resource/" ((fromString . show . entityId) resource)
resourceLink resource html = link (resourceUri resource) html

resourceHeading = h1_ . toHtml . resourceTitle . entityVal

resourceConceptsHeading rel = h2_ ("Concepts " `mappend` (fromString . show) rel)
resourceConceptsMissing rel = p_ ("There are no concepts that " `mappend` (fromString . show) rel `mappend` " this resource")
