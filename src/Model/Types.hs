{-# LANGUAGE TemplateHaskell #-}

module Model.Types where

import Database.Persist.TH (derivePersistField)

data RelationshipType = Taught | Required
    deriving (Eq, Show, Read)
derivePersistField "RelationshipType"
