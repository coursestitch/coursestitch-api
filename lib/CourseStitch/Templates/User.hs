{-# LANGUAGE OverloadedStrings #-}

module CourseStitch.Templates.User where

import CourseStitch.Templates.Utils

users :: [Entity User] -> Html ()
users us = unorderedList $ map userSimple us

user :: Entity User -> Html ()
user user = article_ $ userDetailed user

userSimple :: Entity User -> Html ()
userSimple u = userLink u

userDetailed :: Entity User -> Html ()
userDetailed u = userTitle u

userTitle u = h1_ $ "User " <> toHtml (uName u)
userLink u = link (userUri u) (toHtml (uName u))
userUri u = "/user/" <> (uName u)
uName u = userName $ entityVal u
