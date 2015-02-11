{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Handlers.Root where

import Data.Monoid ((<>))
import CourseStitch.Models (userName)
import CourseStitch.Models.RunDB
import CourseStitch.Handlers.User (getLoggedInUser)
import Database.Persist (entityVal)
import Data.Text.Lazy (fromStrict)
import Web.Scotty (ActionM, text)
import qualified Templates
import CourseStitch.Handlers.Utils (template)

root :: RunDB -> ActionM ()
root runDB = do
    maybeUser <- getLoggedInUser runDB
    case maybeUser of
        Nothing -> text "Course stitch"
        Just u  -> template $ Templates.logoutForm
