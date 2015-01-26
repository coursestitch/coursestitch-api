{-# LANGUAGE OverloadedStrings #-}

module Template.Concept where

import Data.Monoid (mconcat)

import Lucid
import Model

import Template.Template

concepts :: [Concept] -> Html ()
concepts cs = unorderedList $ map conceptSimple cs

conceptSimple :: Concept -> Html ()
conceptSimple concept = do
    conceptHeading concept

conceptHeading = h1_ . toHtml . conceptTitle

