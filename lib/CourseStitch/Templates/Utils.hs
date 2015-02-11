{-# LANGUAGE OverloadedStrings #-}

module CourseStitch.Templates.Utils (
    module Lucid,
    module Data.Monoid,
    module Data.Text,
    module Data.String,
    module CourseStitch.Models,
    module CourseStitch.Templates.Utils
) where

import Lucid
import Data.Monoid (mappend, (<>), mconcat)
import Data.Text (Text)
import Data.String (fromString)

import CourseStitch.Models

import Data.Maybe (fromMaybe)

unorderedList :: [Html ()] -> Html ()
unorderedList = ul_ . mconcat . (map li_)

link :: Monad m => Text -> HtmlT m () -> HtmlT m ()
link url html = a_ [href_ url] html

input :: Text -> Text -> Maybe Text -> Html ()
input label name value = p_ $ do
    label_ $ toHtml label
    " "
    input_ [name_ name, value_ $ fromMaybe "" value]

textInput :: Text -> Text -> Maybe Text -> Html ()
textInput label name value = p_ $ do
    label_ $ toHtml label
    " "
    textarea_ [name_ name] (toHtml $ fromMaybe "" value)
