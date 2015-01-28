{-# LANGUAGE OverloadedStrings #-}

module Template.Concept where

import Data.String (fromString)
import Data.Monoid (mappend, mconcat)

import Lucid
import Model
import Database.Persist (Entity, entityVal)

import Template.Template
import  {-# SOURCE #-} Template.Resource (resourceSimple)

concepts :: [Entity Concept] -> Html ()
concepts cs = unorderedList $ map conceptSimple cs

concept :: Entity Concept  -> [(RelationshipType, [Entity Resource])] -> Html ()
concept concept resources = article_ $ conceptDetailed concept resources

conceptSimple :: Entity Concept -> Html ()
conceptSimple concept = do
    conceptLink concept $ conceptHeading concept

conceptDetailed :: Entity Concept -> [(RelationshipType, [Entity Resource])] -> Html ()
conceptDetailed concept rels = do
    conceptLink concept $ conceptHeading concept
    mconcat $ map (uncurry conceptResources) rels

conceptResources :: RelationshipType -> [Entity Resource] -> Html ()
conceptResources rel resources = do
    conceptResourcesHeading rel
    case resources of
        [] -> conceptResourcesMissing rel
        resources -> unorderedList $ map resourceSimple resources


conceptUri concept = mappend "/concept/" (conceptTitle $ entityVal concept)
conceptLink concept html = link (conceptUri concept) html

conceptHeading = h1_ . toHtml . conceptTitle . entityVal

conceptResourcesHeading rel = h2_ ((fromString . show) rel `mappend` " by")
conceptResourcesMissing rel = p_ ("There are no resources that " `mappend` (fromString . show) rel `mappend` " this concept")
