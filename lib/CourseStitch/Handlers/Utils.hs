{-# LANGUAGE OverloadedStrings #-}

module CourseStitch.Handlers.Utils (
    -- Export common symbols used in most handlers.
    module Control.Monad.IO.Class,
    module Web.Scotty,
    module Database.Persist.Sql,
    module CourseStitch.Models,

    -- And export all symbols defined in this module.
    module CourseStitch.Handlers.Utils
) where

-- These imports are re-exported.
import Control.Monad.IO.Class (liftIO)
import Web.Scotty (ActionM, text, param, status)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import CourseStitch.Models
import CourseStitch.Models.Queries hiding (relationships)

-- Private imports.
import Data.Monoid (mconcat)
import Network.HTTP.Types.Status (status409, status404, status400)

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