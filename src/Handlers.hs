module Handlers where

import Control.Monad.IO.Class (liftIO)
import Data.String (fromString)
import Web.Scotty (ActionM, text)

import Database.Persist.Sql (ConnectionPool, runSqlPool)

import Model
import Model.Queries

root :: ConnectionPool -> ActionM ()
root pool = do
    topics <- liftIO $ runSqlPool getTopics pool
    text (fromString . show $ topics)
