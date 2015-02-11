{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Handlers.User where

import CourseStitch.Handlers.Utils
import qualified Templates

signupForm :: RunDB -> ActionM ()
signupForm runDB = template $ Templates.signupForm

loginForm :: RunDB -> ActionM ()
loginForm runDB = template $ Templates.loginForm
