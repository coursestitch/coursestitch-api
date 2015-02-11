{-# LANGUAGE OverloadedStrings #-}

module Templates.Website where

import Lucid

import CourseStitch.Models
import Database.Persist (Entity)

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
