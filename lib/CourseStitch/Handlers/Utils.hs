{-# LANGUAGE OverloadedStrings #-}

module CourseStitch.Handlers.Utils (
    -- Export common symbols used in most handlers.
    module Text.Read,
    module Control.Monad.IO.Class,
    module Web.Scotty,
    module Database.Persist.Sql,
    module CourseStitch.Models,

    -- And export all symbols defined in this module.
    module CourseStitch.Handlers.Utils
) where

-- These imports are re-exported.
import Text.Read (readMaybe)
import Control.Monad.IO.Class (liftIO)
import Web.Scotty (ActionM, text, param, status)
import Database.Persist.Sql (toSqlKey, ConnectionPool, runSqlPool)
import CourseStitch.Models

-- Private imports.
import Data.Monoid (mconcat)
import Network.HTTP.Types.Status (status500, status409, status404, status403, status400)

import Web.Scotty (ActionM, raw, setHeader)
import Lucid (Html, renderBS)

template :: Html () -> ActionM ()
template html = do
    setHeader "Content-Type" "text/html"
    raw . renderBS $ html

conflict409 msg = do
    status status409
    text msg

notFound404 entity = do
    status status404
    text $ mconcat ["No ", entity, " found"]

badRequest400 msg = do
    status status400
    text msg

forbidden403 msg = do
    status status403
    text msg

error500 msg = do
    status status500
    text msg
