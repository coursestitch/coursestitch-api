{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Handlers.User where

import CourseStitch.Handlers.Utils
import qualified Templates

loginForm :: RunDB -> ActionM ()
loginForm runDB = template $ Templates.loginForm
