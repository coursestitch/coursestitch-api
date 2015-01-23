module Template (
    module Template.Topic,
    template
) where

import Template.Topic

import Web.Scotty (ActionM, raw)
import Lucid (Html, renderBS)

template :: Html () -> ActionM ()
template = raw . renderBS
