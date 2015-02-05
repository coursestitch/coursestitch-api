{-# LANGUAGE OverloadedStrings #-}

module Template (
    module Template.Resource,
    module Template.Relationship,
    module Template.Concept,
    module Template.Topic,
    module Template.User,
    template
) where

import Template.Resource
import Template.Relationship
import Template.Concept
import Template.Topic
import Template.User

import Web.Scotty (ActionM, raw, setHeader)
import Lucid (Html, renderBS)

template :: Html () -> ActionM ()
template html = do
    setHeader "Content-Type" "text/html"
    raw . renderBS $ html
