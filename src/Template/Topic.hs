{-# LANGUAGE OverloadedStrings #-}

module Template.Topic where

import Lucid
import Model

topics :: [Topic] -> Html ()
topics ts = p_ "Hello world"
