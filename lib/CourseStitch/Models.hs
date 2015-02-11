module CourseStitch.Models (
    module CourseStitch.Models.Tables,
    module CourseStitch.Models.Types,
    entityId
) where

import CourseStitch.Models.Tables
import CourseStitch.Models.Types


import Data.Int (Int64)

import Database.Persist (entityKey)
import Database.Persist.Sql (unSqlBackendKey)

entityId = unSqlBackendKey . unResourceKey . entityKey
