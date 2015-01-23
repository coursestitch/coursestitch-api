{-# LANGUAGE OverloadedStrings #-}

module Template.Concept where

import Data.Monoid (mconcat)

import Lucid
import Model

conceptSimple :: Concept -> Html ()
conceptSimple concept = do
    conceptHeading concept

conceptHeading = h1_ . toHtml . conceptTitle

