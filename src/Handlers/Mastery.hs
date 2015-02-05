{-# LANGUAGE OverloadedStrings #-}

module Handlers.Mastery where

import Handlers.Handlers
import qualified Template
import Handlers.User (authenticate)
import Handlers.Resource (resourceAction)
import Handlers.Concept (conceptAction)

import Database.Persist (Entity, entityVal)
import Crypto.BCrypt (validatePassword)
import Web.Scotty.Cookie (getCookie, setSimpleCookie)
import Network.HTTP.Types.Status (forbidden403)
import Data.Maybe (isJust)
import Data.Text.Lazy (pack)
import Data.Monoid (mconcat)

masteries :: ConnectionPool -> ActionM ()
masteries pool = do
    (rm, cm) <- liftIO $ runSqlPool getMasteries pool
    let shownRM = map (pack . show . entityVal) rm
        shownCM = map (pack . show . entityVal) cm
    text $ mconcat $ shownRM ++ shownCM

resourceMasteryCreate :: ConnectionPool -> ActionM ()
resourceMasteryCreate pool = authenticate pool $ \user ->
    resourceAction' pool $ \resource -> do
        maybeMastery <- liftIO $ runSqlPool (newResourceMastery user resource) pool
        case maybeMastery of
            Nothing -> conflict409 "Mastery already exists"
            Just m  -> text "created"

resourceMasteryDelete :: ConnectionPool -> ActionM ()
resourceMasteryDelete pool = authenticate pool $ \user ->
    resourceAction' pool $ \resource ->
        liftIO $ runSqlPool (deleteResourceMastery user resource) pool

conceptMasteryCreate :: ConnectionPool -> ActionM ()
conceptMasteryCreate pool = authenticate pool $ \user ->
    conceptAction' pool $ \concept -> do
        maybeMastery <- liftIO $ runSqlPool (newConceptMastery user concept) pool
        case maybeMastery of
            Nothing -> conflict409 "Mastery already exists"
            Just m  -> text "created"

conceptMasteryDelete :: ConnectionPool -> ActionM ()
conceptMasteryDelete pool = authenticate pool $ \user ->
    conceptAction' pool $ \concept ->
        liftIO $ runSqlPool (deleteConceptMastery user concept) pool

resourceAction' pool action = resourceAction pool (\_ res _   -> action res)
conceptAction'  pool action = conceptAction  pool (\_ con _ _ -> action con)
