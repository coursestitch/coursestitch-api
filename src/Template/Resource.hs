{-# LANGUAGE OverloadedStrings #-}

module Template.Resource where

import Data.String (fromString)
import Data.Monoid (mappend, mconcat)

import Lucid
import Model

import Template.Template
import Template.Concept (conceptSimple)

resources :: [Resource] -> Html ()
resources cs = unorderedList $ map resourceSimple cs

resource :: Resource -> [(RelationshipType, [Concept])] -> Html ()
resource resource concepts = article_ $ resourceDetailed resource concepts

resourceSimple :: Resource -> Html ()
resourceSimple resource = do
    resourceLink resource $ resourceHeading resource

resourceDetailed :: Resource -> [(RelationshipType, [Concept])] -> Html ()
resourceDetailed resource rels = do
    resourceLink resource $ resourceHeading resource
    mconcat $ map (uncurry resourceConcepts) rels
    
resourceConcepts :: RelationshipType -> [Concept] -> Html ()
resourceConcepts rel concepts = do
    resourceConceptsHeading rel
    case concepts of
        [] -> resourceConceptsMissing rel
        concepts -> unorderedList $ map conceptSimple concepts


resourceUri resource = mappend "/resource/" (resourceTitle resource)
resourceLink resource html = link (resourceUri resource) html

resourceHeading = h1_ . toHtml . resourceTitle

resourceConceptsHeading rel = h2_ ("Concepts " `mappend` (fromString . show) rel)
resourceConceptsMissing rel = p_ ("There are no concepts that " `mappend` (fromString . show) rel `mappend` " this resource")
