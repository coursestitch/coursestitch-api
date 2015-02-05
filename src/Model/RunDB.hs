{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Model.RunDB where

import Control.Monad.IO.Class (liftIO)
import Database.Persist.Sql (ConnectionPool, runSqlPool, SqlPersistT)
import Web.Scotty (ActionM)

type RunDB = forall a. SqlPersistT IO a -> ActionM a
runDB' :: ConnectionPool -> RunDB
runDB' pool q = liftIO $ runSqlPool q pool
