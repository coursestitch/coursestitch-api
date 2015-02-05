{-# LANGUAGE OverloadedStrings #-}

module Handlers.Mastery where

import Handlers.Handlers
import qualified Template
import Handlers.User (authenticate)
import Handlers.Resource (resourceAction)

import Database.Persist (Entity, entityVal)
import Crypto.BCrypt (validatePassword)
import Web.Scotty.Cookie (getCookie, setSimpleCookie)
import Network.HTTP.Types.Status (forbidden403)
import Data.Maybe (isJust)
import Data.Text.Lazy (pack)
import Data.Monoid (mconcat)

masteries :: ConnectionPool -> ActionM ()
masteries pool = do
    masteries <- liftIO $ runSqlPool getMasteries pool
    text $ mconcat $ map (pack . show . entityVal) masteries

masteryCreate :: ConnectionPool -> ActionM ()
masteryCreate pool = authenticate pool $ \user ->
    resourceAction' pool $ \resource -> do
        maybeMastery <- liftIO $ runSqlPool (newMastery user resource) pool
        case maybeMastery of
            Nothing -> conflict409 "Mastery already exists"
            Just m  -> text "created"

masteryDelete :: ConnectionPool -> ActionM ()
masteryDelete pool = authenticate pool $ \user ->
    resourceAction' pool $ \resource ->
        liftIO $ runSqlPool (deleteMastery user resource) pool

resourceAction' pool action = resourceAction pool (\_ res _ -> action res)
