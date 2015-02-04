{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Handlers.Handlers (
    -- Export common symbols used in most handlers.
    module Control.Monad.IO.Class,
    module Web.Scotty,
    module Database.Persist.Sql,
    module Model,
    module Model.Queries,
    module Template,

    -- And export all symbols defined in this module.
    module Handlers.Handlers
) where

import Text.Read (readMaybe)

-- These imports are re-exported.
import Control.Monad.IO.Class (liftIO)
import Web.Scotty (ActionM, text, param, status)
import Database.Persist.Sql (ConnectionPool, runSqlPool, SqlPersistT, PersistEntityBackend, )
import Model
import Model.Queries
import qualified Template
import Template (template)

-- Private imports.
import Data.Monoid (mconcat)
import Database.Persist (PersistEntity, Entity)
import Network.HTTP.Types.Status (status409, status404, status400)

-- Status Handlers
conflict409 msg = do
    status status409
    text msg

notFound404 entity = do
    status status404
    text $ mconcat ["No ", entity, " found"]

badRequest400 msg = do
    status status400
    text msg

-- Entity Handlers
entities pool = do
    entityList <- liftIO $ runSqlPool getEntities pool
    template $ Template.entities entityList
    return entityList

entity pool = do
    id <- param "id"
    
    case readMaybe id of
        Nothing -> do
            badRequest400 "id should be an integer"
            return Nothing
        Just id -> do
            entity <- liftIO $ runSqlPool (getEntity id) pool
            case entity of
                Nothing     -> notFound404 "entity"
                Just entity -> template $ Template.entity entity
            return entity
