{-# LANGUAGE OverloadedStrings #-}

module Template (
    module Template.Topic,
    module Template.Concept,
    template
) where

import Template.Topic
import Template.Concept

import Web.Scotty (ActionM, raw, setHeader)
import Lucid (Html, renderBS)

template :: Html () -> ActionM ()
template html = do
    setHeader "Content-Type" "text/html"
    raw . renderBS $ html
