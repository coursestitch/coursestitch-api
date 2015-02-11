{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module CourseStitch.Handlers.User where

import CourseStitch.Handlers.Utils
import qualified CourseStitch.Templates as Templates
import CourseStitch.Models.RunDB

import Crypto.BCrypt (validatePassword, hashPasswordUsingPolicy, fastBcryptHashingPolicy)
import Web.Scotty.Cookie (getCookie, setSimpleCookie)
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

userCreate :: RunDB -> ActionM ()
userCreate runDB = do
    userName <- param "name"
    userPass <- param "pass"
    existingUser <- runDB $ getUser userName
    case existingUser of
        Just u  -> nameTaken
        Nothing -> do
            maybeHash <- liftIO $ hashPassword userPass
            case maybeHash of
                Nothing   -> error500 "Hash went wrong"
                Just hash -> do
                    newUser <- runDB $ newUser (User userName hash)
                    case newUser of
                        Nothing -> nameTaken
                        Just u  -> template $ Templates.userCreated u
    where hashPassword = hashPasswordUsingPolicy fastBcryptHashingPolicy
          nameTaken = conflict409 "A user with that name already exists"

isLoggedIn :: RunDB -> ActionM Bool
isLoggedIn runDB = fmap isJust (getLoggedInUser runDB)

getLoggedInUser :: RunDB -> ActionM (Maybe (Entity User))
getLoggedInUser runDB = do
    maybeToken <- getCookie "session"
    case maybeToken of
        Nothing -> return Nothing
        Just t  -> runDB (getUserForToken (Token t))

whenAuthenticated :: RunDB -> (Entity User -> ActionM ()) -> ActionM ()
whenAuthenticated runDB action = authenticate runDB fail action
    where fail = forbidden403 "must be logged in"

whenAuthenticated_ :: RunDB -> ActionM () -> ActionM ()
whenAuthenticated_ runDB action = whenAuthenticated runDB (const action)

authenticate :: RunDB -> ActionM () -> (Entity User -> ActionM ()) -> ActionM ()
authenticate runDB fail success = do
    maybeUser <- getLoggedInUser runDB
    case maybeUser of
        Nothing -> fail
        Just u  -> success u

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
            else forbidden403 "incorrect password"

logout :: RunDB -> ActionM ()
logout runDB = do
    maybeToken <- getCookie "session"
    case maybeToken of
        Nothing -> return ()
        Just t  -> do
            runDB (deleteSessions (Token t))
            setSimpleCookie "session" ""
            text "logged out"
