{-# LANGUAGE TemplateHaskell #-}

module Model.Types where

import Data.Text (Text)
import Database.Persist.TH (derivePersistField)

data RelationshipType = Taught | Required
    deriving (Eq, Show, Read)
derivePersistField "RelationshipType"

newtype Token = Token Text
    deriving (Eq, Show, Read)
derivePersistField "Token"
