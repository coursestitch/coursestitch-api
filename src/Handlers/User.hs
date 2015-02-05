{-# LANGUAGE OverloadedStrings #-}

module Handlers.User where

import Handlers.Handlers
import qualified Template

import Database.Persist (Entity, entityVal)
import Crypto.BCrypt (validatePassword)
import Web.Scotty.Cookie (getCookie, setSimpleCookie)
import Network.HTTP.Types.Status (forbidden403)
import Data.Maybe (isJust)

users :: ConnectionPool -> ActionM ()
users pool = do
    userList <- liftIO $ runSqlPool getUsers pool
    template $ Template.users userList

user :: ConnectionPool -> ActionM ()
user pool = do
    userName <- param "user"
    userEntity <- liftIO $ runSqlPool (getUser userName) pool
    case userEntity of
        Nothing -> notFound404 "user"
        Just u  -> template $ Template.user u

getLoggedInUser :: ConnectionPool -> ActionM (Maybe (Entity User))
getLoggedInUser pool = do
    maybeToken <- getCookie "session"
    case maybeToken of
        Nothing -> return Nothing
        Just t  -> liftIO $ runSqlPool (getUserForToken (Token t)) pool

isLoggedIn :: ConnectionPool -> ActionM Bool
isLoggedIn pool = fmap isJust (getLoggedInUser pool)

authenticate :: ConnectionPool -> (Entity User -> ActionM ()) -> ActionM ()
authenticate pool action = do
    maybeUser <- getLoggedInUser pool
    case maybeUser of
        Nothing -> status forbidden403 >> text "must be logged in"
        Just u  -> action u

authenticate_ :: ConnectionPool -> ActionM () -> ActionM ()
authenticate_ pool action = authenticate pool (const action)

loginForm :: ConnectionPool -> ActionM ()
loginForm pool = template $ Template.loginForm

login :: ConnectionPool -> ActionM ()
login pool = do
    userName <- param "name"
    userPass <- param "pass"
    userEntity <- liftIO $ runSqlPool (getUser userName) pool
    case userEntity of
        Nothing -> notFound404 "user"
        Just u  -> if validatePassword (userHash $ entityVal u) userPass
            then do
                Token t <- liftIO $ runSqlPool (newSession u) pool
                setSimpleCookie "session" t
                text "logged in"
            else status forbidden403 >> text "incorrect password"

logout :: ConnectionPool -> ActionM ()
logout pool = do
    maybeToken <- getCookie "session"
    case maybeToken of
        Nothing -> return ()
        Just t  -> do
            liftIO $ runSqlPool (deleteSessions (Token t)) pool
            setSimpleCookie "session" ""
            text "logged out"
