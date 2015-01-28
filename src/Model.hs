module Model (
    module Model.Tables,
    module Model.Types,
    entityId
 ) where

import Model.Tables
import Model.Types


import Data.Int (Int64)

import Database.Persist (entityKey)
import Database.Persist.Sql (unSqlBackendKey)

entityId = unSqlBackendKey . unResourceKey . entityKey
