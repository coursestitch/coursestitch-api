{-# LANGUAGE OverloadedStrings #-}

module Template.Template where

import Data.Monoid (mconcat)

import Lucid

unorderedList :: [Html ()] -> Html ()
unorderedList = ul_ . mconcat . (map li_)
