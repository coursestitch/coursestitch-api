{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Handlers.Mastery where

import Handlers.Handlers
import qualified Template
import Model.RunDB
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

masteries :: RunDB -> ActionM ()
masteries runDB = do
    (rm, cm) <- runDB getMasteries
    let shownRM = map (pack . show . entityVal) rm
        shownCM = map (pack . show . entityVal) cm
    text $ mconcat $ shownRM ++ shownCM

resourceMasteryCreate :: RunDB -> ActionM ()
resourceMasteryCreate runDB = authenticate runDB $ \user ->
    resourceAction' runDB $ \resource -> do
        maybeMastery <- runDB $ newResourceMastery user resource
        text "created"

resourceMasteryDelete :: RunDB -> ActionM ()
resourceMasteryDelete runDB = authenticate runDB $ \user ->
    resourceAction' runDB $ \resource ->
        runDB $ deleteResourceMastery user resource

conceptMasteryCreate :: RunDB -> ActionM ()
conceptMasteryCreate runDB = authenticate runDB $ \user ->
    conceptAction' runDB $ \concept -> do
        maybeMastery <- runDB $ newConceptMastery user concept
        text "created"

conceptMasteryDelete :: RunDB -> ActionM ()
conceptMasteryDelete runDB = authenticate runDB $ \user ->
    conceptAction' runDB $ \concept ->
        runDB $ deleteConceptMastery user concept

resourceAction' :: RunDB -> (Entity Resource -> ActionM ()) -> ActionM ()
conceptAction'  :: RunDB -> (Entity Concept  -> ActionM ()) -> ActionM ()
resourceAction' runDB action = resourceAction runDB (\_ res _   -> action res)
conceptAction'  runDB action = conceptAction  runDB (\_ con _ _ -> action con)
