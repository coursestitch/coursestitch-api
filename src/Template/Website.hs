{-# LANGUAGE OverloadedStrings #-}

module Template.Website where

import Lucid

import Model
import Database.Persist (Entity)

import Template.Resource
import Template.Concept

page :: Html () -> Html ()
page body = do
    html_ $ do
        head_ $ do
            title_ "course stitch"
            script_ [src_ "//use.typekit.net/wlh0kqu.js"] ("" :: String)
            script_ [] ("try{Typekit.load();}catch(e){}" :: String)
            css_ "/css/reset.css"
            css_ "/css/main.css"
        body_ body
    
    where css_ url = link_ [type_ "text/css", rel_ "stylesheet", href_ url]

resourcePage :: Entity Resource -> [(RelationshipType, [Entity Concept])] -> Html()
resourcePage r rels = page $ resourceDetailed r rels

conceptPage :: Entity Concept -> Maybe (Entity Topic) -> [(RelationshipType, [Entity Resource])] -> Html()
conceptPage c t rs = page $ concept c t rs
