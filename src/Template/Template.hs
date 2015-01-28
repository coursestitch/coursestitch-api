{-# LANGUAGE OverloadedStrings #-}

module Template.Template where

import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)

import Lucid

unorderedList :: [Html ()] -> Html ()
unorderedList = ul_ . mconcat . (map li_)

link :: Text -> Html () -> Html ()
link url html = a_ [href_ url] html

input :: Text -> Text -> Maybe Text -> Html ()
input label name value = p_ $ do
    label_ $ toHtml label
    " "
    input_ [name_ name, value_ $ fromMaybe "" value]
