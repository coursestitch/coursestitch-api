{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import Control.Monad.IO.Class (liftIO)
import Data.String (fromString)
import Web.Scotty (ActionM, text, param)

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

topic :: ConnectionPool -> ActionM ()
topic pool = do
    title <- param "topic"
    topic <- liftIO $ runSqlPool (getTopic title) pool
    case topic of Nothing    -> text "No topic found"
                  Just topic -> template $ Template.topic topic []
