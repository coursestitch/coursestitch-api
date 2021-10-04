{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module CourseStitch.Models.RunDB where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Database.Persist.Sql (ConnectionPool, runSqlPool, SqlPersistT)
import Web.Scotty (ActionM)

type RunDB = forall m. MonadIO m => (forall a. SqlPersistT IO a -> m a)
runDB' :: ConnectionPool -> RunDB
runDB' pool q = liftIO $ runSqlPool q pool
