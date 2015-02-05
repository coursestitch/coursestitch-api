{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Handlers.User where

import Handlers.Handlers
import qualified Template
import Model.RunDB

import Database.Persist (Entity, entityVal)
import Crypto.BCrypt (validatePassword)
import Web.Scotty.Cookie (getCookie, setSimpleCookie)
import Network.HTTP.Types.Status (forbidden403)
import Data.Maybe (isJust)

users :: RunDB -> ActionM ()
users runDB = do
    userList <- runDB getUsers
    template $ Template.users userList

user :: RunDB -> ActionM ()
user runDB = do
    userName <- param "user"
    userEntity <- runDB (getUser userName)
    case userEntity of
        Nothing -> notFound404 "user"
        Just u  -> template $ Template.user u

getLoggedInUser :: RunDB -> ActionM (Maybe (Entity User))
getLoggedInUser runDB = do
    maybeToken <- getCookie "session"
    case maybeToken of
        Nothing -> return Nothing
        Just t  -> runDB (getUserForToken (Token t))

isLoggedIn :: RunDB -> ActionM Bool
isLoggedIn runDB = fmap isJust (getLoggedInUser runDB)

authenticate :: RunDB -> (Entity User -> ActionM ()) -> ActionM ()
authenticate runDB action = do
    maybeUser <- getLoggedInUser runDB
    case maybeUser of
        Nothing -> status forbidden403 >> text "must be logged in"
        Just u  -> action u

authenticate_ :: RunDB -> ActionM () -> ActionM ()
authenticate_ runDB action = authenticate runDB (const action)

loginForm :: RunDB -> ActionM ()
loginForm runDB = template $ Template.loginForm

login :: RunDB -> ActionM ()
login runDB = do
    userName <- param "name"
    userPass <- param "pass"
    userEntity <- runDB (getUser userName)
    case userEntity of
        Nothing -> notFound404 "user"
        Just u  -> if validatePassword (userHash $ entityVal u) userPass
            then do
                Token t <- runDB (newSession u)
                setSimpleCookie "session" t
                text "logged in"
            else status forbidden403 >> text "incorrect password"

logout :: RunDB -> ActionM ()
logout runDB = do
    maybeToken <- getCookie "session"
    case maybeToken of
        Nothing -> return ()
        Just t  -> do
            runDB (deleteSessions (Token t))
            setSimpleCookie "session" ""
            text "logged out"
