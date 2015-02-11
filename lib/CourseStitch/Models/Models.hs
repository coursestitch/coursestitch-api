{-# LANGUAGE NoMonomorphismRestriction #-}

module CourseStitch.Models.Models where

import Database.Persist (toBackendKey, entityKey)
import Database.Persist.Sql (unSqlBackendKey)

entityId = unSqlBackendKey . toBackendKey . entityKey
