{-# LANGUAGE OverloadedStrings #-}

module Handlers.User where

import Handlers.Handlers
import qualified Template

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

login :: ConnectionPool -> ActionM ()
login pool = do
    userName <- param "name"
    --userPwd  <- param "pwd"
    userEntity <- liftIO $ runSqlPool (getUser userName) pool
    case userEntity of
        Nothing -> notFound404 "user"
        Just u  -> do
            -- Hash password with stored salt
            -- If match with stored hash, create session
            -- Otherwise, boom
            text ""

logout :: ConnectionPool -> ActionM ()
logout pool = do
    token <- fmap Token $ param "token"
    sessionEntity <- liftIO $ runSqlPool (deleteSessions token) pool
    text ""
