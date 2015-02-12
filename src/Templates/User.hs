{-# LANGUAGE OverloadedStrings #-}

module Templates.User where

import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types.Method (methodGet, methodPost, methodPut, methodDelete)

import CourseStitch.Templates.Utils
import CourseStitch.Templates.Topic

signupForm :: Html ()
signupForm = form_ [action_ "/user", method_ post] $ do
    fieldset_ $ do
        input "Username" "name" Nothing
        input "Password" "pass" Nothing
    input_ [type_ "submit"]
    where post = decodeUtf8 methodPost

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
