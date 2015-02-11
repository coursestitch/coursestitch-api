{-# LANGUAGE NoMonomorphismRestriction #-}

module CourseStitch.Models.Models (
    Entity, entityVal, entityKey,
    module CourseStitch.Models.Models
) where

import Database.Persist (Entity, entityVal, entityKey, toBackendKey)
import Database.Persist.Sql (unSqlBackendKey)

entityId = unSqlBackendKey . toBackendKey . entityKey
