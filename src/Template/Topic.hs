{-# LANGUAGE OverloadedStrings #-}

module Template.Topic where

import Data.Monoid (mconcat)

import Lucid
import Model

topics :: [Topic] -> Html ()
topics ts = ul_ $ mconcat $ map (li_ . topicSimple) ts

topicSimple :: Topic -> Html ()
topicSimple topic = do
    h1_ $ toHtml $ topicTitle topic
    p_ $ toHtml $ topicSummary topic
