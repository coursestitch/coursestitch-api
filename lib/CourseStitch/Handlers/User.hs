{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module CourseStitch.Handlers.User where

import CourseStitch.Handlers.Handlers
import qualified CourseStitch.Templates as Templates
import CourseStitch.Models.RunDB

import Database.Persist (Entity, entityVal)
import Crypto.BCrypt (validatePassword)
import Web.Scotty.Cookie (getCookie, setSimpleCookie)
import Network.HTTP.Types.Status (forbidden403)
import Data.Maybe (isJust)

users :: RunDB -> ActionM ()
users runDB = do
    userList <- runDB getUsers
    template $ Templates.users userList

user :: RunDB -> ActionM ()
user runDB = do
    userName <- param "user"
    userEntity <- runDB (getUser userName)
    case userEntity of
        Nothing -> notFound404 "user"
        Just u  -> template $ Templates.user u

getLoggedInUser :: RunDB -> ActionM (Maybe (Entity User))
getLoggedInUser runDB = do
    maybeToken <- getCookie "session"
    case maybeToken of
        Nothing -> return Nothing
        Just t  -> runDB (getUserForToken (Token t))

isLoggedIn :: RunDB -> ActionM Bool
isLoggedIn runDB = fmap isJust (getLoggedInUser runDB)

whenAuthenticated :: RunDB -> (Entity User -> ActionM ()) -> ActionM ()
whenAuthenticated runDB action = authenticate runDB fail action
    where fail = status forbidden403 >> text "must be logged in"

whenAuthenticated_ :: RunDB -> ActionM () -> ActionM ()
whenAuthenticated_ runDB action = whenAuthenticated runDB (const action)

authenticate :: RunDB -> ActionM () -> (Entity User -> ActionM ()) -> ActionM ()
authenticate runDB fail success = do
    maybeUser <- getLoggedInUser runDB
    case maybeUser of
        Nothing -> fail
        Just u  -> success u

loginForm :: RunDB -> ActionM ()
loginForm runDB = template $ Templates.loginForm

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
