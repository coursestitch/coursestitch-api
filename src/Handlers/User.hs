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
