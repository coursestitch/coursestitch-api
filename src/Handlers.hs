module Handlers where

import Control.Monad.IO.Class (liftIO)
import Data.String (fromString)
import Web.Scotty (ActionM, text)

import Database.Persist.Sql (ConnectionPool, runSqlPool)

import Model
import Model.Queries

import qualified Template
import Template (template)

root :: ConnectionPool -> ActionM ()
root pool = do
    topics <- liftIO $ runSqlPool getTopics pool
    text (fromString . show $ topics)

topics :: ConnectionPool -> ActionM ()
topics pool = do
    topicList <- liftIO $ runSqlPool getTopics pool
    template $ Template.topics topicList
