{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Template (
    module Template.Resource,
    module Template.Concept,
    module Template.Topic,
    module Template.User,
    HtmlShow,
    entities,
    template
) where

import Data.Monoid (mconcat)

import Template.Resource
import Template.Concept
import Template.Topic
import Template.User

import Model
import Database.Persist (Entity)

import Web.Scotty (ActionM, raw, setHeader)
import Lucid (Html, renderBS, article_)

template :: Html () -> ActionM ()
template html = do
    setHeader "Content-Type" "text/html"
    raw . renderBS $ html

-- Pages
class HtmlShow a where
    simpleHtml :: a -> Html ()
    detailedHtml :: a -> Html ()

instance HtmlShow (Entity Resource) where
    simpleHtml resource = resourceLink resource $ resourceHeading resource

instance HtmlShow (Entity Concept) where
    simpleHtml concept = conceptLink concept $ conceptHeading concept

instance HtmlShow (Entity Topic) where
    simpleHtml = topicSimple

instance HtmlShow (Entity User) where
    simpleHtml = userSimple

entities = article_ . (mconcat . map simpleHtml)
