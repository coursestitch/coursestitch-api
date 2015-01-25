{-# LANGUAGE OverloadedStrings #-}

module Template.Template where

import Data.Text (Text)
import Data.Monoid (mconcat)

import Lucid

unorderedList :: [Html ()] -> Html ()
unorderedList = ul_ . mconcat . (map li_)

link :: Text -> Html () -> Html ()
link url html = a_ [href_ url] html
