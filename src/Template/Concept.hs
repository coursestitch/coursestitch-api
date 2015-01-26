{-# LANGUAGE OverloadedStrings #-}

module Template.Concept where

import Data.Monoid (mappend)

import Lucid
import Model

import Template.Template

concepts :: [Concept] -> Html ()
concepts cs = unorderedList $ map conceptSimple cs

concept :: Concept -> Html ()
concept concept = article_ $ conceptDetailed concept

conceptSimple :: Concept -> Html ()
conceptSimple concept = do
    conceptLink concept $ conceptHeading concept

conceptDetailed :: Concept -> Html ()
conceptDetailed concept = do
    conceptLink concept $ conceptHeading concept

conceptUri concept = mappend "/concept/" (conceptTitle concept)
conceptLink concept html = link (conceptUri concept) html

conceptHeading = h1_ . toHtml . conceptTitle
