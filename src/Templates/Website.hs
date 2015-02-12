{-# LANGUAGE OverloadedStrings #-}

module Templates.Website where

import Data.String (fromString)

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

typeahead :: String -> String -> Html ()
typeahead aheadUri placeholder = do
    input_ [ class_ "typeahead", placeholder_ $ fromString placeholder
           , data_ "url" $ fromString aheadUri]

    script_ [src_ "//cdnjs.cloudflare.com/ajax/libs/jquery/2.1.3/jquery.min.js"] ("" :: String)
    script_ [src_ "/js/typeahead.js"] ("" :: String)
    script_ [src_ "/js/typeahead-input.js"] ("" :: String)
