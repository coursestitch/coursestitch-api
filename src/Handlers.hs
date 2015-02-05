{-# LANGUAGE OverloadedStrings #-}

module Handlers (
    module Handlers.Resource,
    module Handlers.Relationship,
    module Handlers.Concept,
    module Handlers.Topic,
    module Handlers.User,
    module Handlers.Mastery,
    module Handlers.Handlers,
    module Handlers
) where

import Handlers.Resource
import Handlers.Relationship
import Handlers.Concept
import Handlers.Topic
import Handlers.User
import Handlers.Mastery
import Handlers.Handlers

import Data.Monoid ((<>))
import Model (userName)
import Database.Persist (entityVal)
import Data.Text.Lazy (fromStrict)
import Web.Scotty (html)
import qualified Template

root :: ConnectionPool -> ActionM ()
root pool = do
    maybeUser <- getLoggedInUser pool
    case maybeUser of
        Nothing -> text "Course stitch"
        Just u  -> template $ Template.logoutForm
