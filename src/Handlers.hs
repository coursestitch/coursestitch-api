{-# LANGUAGE OverloadedStrings #-}

module Handlers (
    module Handlers.Resource,
    module Handlers.Relationship,
    module Handlers.Concept,
    module Handlers.Topic,
    module Handlers.User,
    module Handlers.Handlers,
    module Handlers
) where

import Handlers.Resource
import Handlers.Relationship
import Handlers.Concept
import Handlers.Topic
import Handlers.User
import Handlers.Handlers

root :: ActionM ()
root = text "Course stitch"
