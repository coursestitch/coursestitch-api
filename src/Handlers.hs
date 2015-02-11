{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Handlers (
    module Handlers.Concept,
    module Handlers.Resource,
    module Handlers
) where

import Handlers.Concept
import Handlers.Resource

import Data.Monoid ((<>))
import CourseStitch.Models (userName)
import CourseStitch.Models.RunDB
import CourseStitch.Handlers.User (getLoggedInUser)
import Database.Persist (entityVal)
import Data.Text.Lazy (fromStrict)
import Web.Scotty (ActionM, text)
import qualified CourseStitch.Templates as Templates
import CourseStitch.Templates (template)

root :: RunDB -> ActionM ()
root runDB = do
    maybeUser <- getLoggedInUser runDB
    case maybeUser of
        Nothing -> text "Course stitch"
        Just u  -> template $ Templates.logoutForm
