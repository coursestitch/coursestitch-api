{-# LANGUAGE OverloadedStrings #-}

module Template.Website where

import Lucid

import Model
import Database.Persist (Entity)

import Template.Resource

page :: Html () -> Html ()
page body = do
    html_ $ do
        head_ $ do
            title_ "course stitch"
            css_ "/css/reset.css"
            css_ "/css/main.css"
        body_ body
    
    where css_ url = link_ [type_ "text/css", rel_ "stylesheet", href_ url]

resourcePage :: Entity Resource -> [(RelationshipType, [Entity Concept])] -> Html()
resourcePage r rels = page $ resource r rels
