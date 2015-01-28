{-# LANGUAGE OverloadedStrings #-}

module Template.Concept where

import Data.String (fromString)
import Data.Monoid (mappend, mconcat)

import Lucid
import Model

import Template.Template

concepts :: [Concept] -> Html ()
concepts cs = unorderedList $ map conceptSimple cs

concept :: Concept  -> [(RelationshipType, [Resource])] -> Html ()
concept concept resources = article_ $ conceptDetailed concept resources

conceptSimple :: Concept -> Html ()
conceptSimple concept = do
    conceptLink concept $ conceptHeading concept

conceptDetailed :: Concept -> [(RelationshipType, [Resource])] -> Html ()
conceptDetailed concept rels = do
    conceptLink concept $ conceptHeading concept
    mconcat $ map (uncurry conceptResources) rels

conceptResources :: RelationshipType -> [Resource] -> Html ()
conceptResources rel resources = do
    conceptResourcesHeading rel
    case resources of
        [] -> conceptResourcesMissing rel
        resources -> unorderedList $ map resourceSimplex resources


conceptUri concept = mappend "/concept/" (conceptTitle concept)
conceptLink concept html = link (conceptUri concept) html

conceptHeading = h1_ . toHtml . conceptTitle

conceptResourcesHeading rel = h2_ ("Resources that " `mappend` (fromString . show) rel)
conceptResourcesMissing rel = p_ ("There are no resources that " `mappend` (fromString . show) rel `mappend` " this concept")


resourceSimplex :: Resource -> Html ()
resourceSimplex resource = do
    resourceLinkx resource $ resourceHeadingx resource

resourceUrix resource = mappend "/resource/" (resourceTitle resource)
resourceLinkx resource html = link (resourceUrix resource) html
resourceHeadingx = h1_ . toHtml . resourceTitle
