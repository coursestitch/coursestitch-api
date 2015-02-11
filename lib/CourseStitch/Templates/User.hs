{-# LANGUAGE OverloadedStrings #-}

module CourseStitch.Templates.User where

import Data.Monoid ((<>))

import Lucid
import CourseStitch.Models
import Database.Persist (Entity, entityVal)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types.Method (methodPost, methodDelete)

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

loginForm :: Html ()
loginForm = form_ [action_ "/session", method_ post] $ do
    fieldset_ $ do
        input "Username" "name" Nothing
        input "Password" "pass" Nothing
    input_ [type_ "submit"]
    where post = decodeUtf8 methodPost

logoutForm :: Html ()
logoutForm = do
    form_ [action_ "/session", method_ delete] $
        input_ [type_ "submit", value_ "Logout"]
    script_ [src_ "/js/form-methods.js"] ("" :: String)
    where delete = decodeUtf8 methodDelete
