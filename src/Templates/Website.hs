{-# LANGUAGE OverloadedStrings #-}

module Templates.Website where

import Data.String (fromString)
import Data.List (intercalate)

import Lucid
import CourseStitch.Templates.Utils

import CourseStitch.Models
import Database.Persist (Entity)

page :: Html () -> Html ()
page body = do
    html_ $ do
        head_ $ do
            title_ "course stitch"
            script_ [src_ "//cdnjs.cloudflare.com/ajax/libs/jquery/2.1.3/jquery.min.js"] ("" :: String)
            script_ [src_ "//use.typekit.net/wlh0kqu.js"] ("" :: String)
            script_ [] ("try{Typekit.load();}catch(e){}" :: String)
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
            css_ "/css/reset.css"
            css_ "/css/main.css"
        header
        body_ body
    
    where css_ url = link_ [type_ "text/css", rel_ "stylesheet", href_ url]

header :: Html ()
header = header_ $ do
    nav_ $ do
        link "/" "course stitch"
        link "/resource" "resources"
        link "/concept" "concepts"
        link "/topic" "topics"

typeahead :: String -> String -> [String] -> Html ()
typeahead aheadUri placeholder classes = do
    input_ [ class_ $ fromString classes', placeholder_ $ fromString placeholder
           , data_ "url" $ fromString aheadUri]

    script_ [src_ "/js/typeahead.js"] ("" :: String)
    script_ [src_ "/js/typeahead-input.js"] ("" :: String)

    where classes' = intercalate " " $ ["typeahead"] ++ classes
