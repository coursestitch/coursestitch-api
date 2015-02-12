{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module CourseStitch.Models.Types where

import Control.Applicative ((<$>))
import Control.Monad (mzero)
import Data.Text (Text)
import Data.String (fromString)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.:))
import Database.Persist.TH (derivePersistField)

data RelationshipType = Taught | Required
    deriving (Eq, Show, Read, Enum)
derivePersistField "RelationshipType"

instance FromJSON RelationshipType where
    parseJSON (Object v) = createRelationshipType <$> (v .: "relationshipType")
    parseJSON _ = mzero

instance ToJSON RelationshipType where
    toJSON = String . fromString . show

createRelationshipType :: String -> RelationshipType
createRelationshipType "Taught"   = Taught
createRelationshipType "Required" = Required
createRelationshipType _          = Taught

newtype Token = Token Text
    deriving (Eq, Show, Read)
derivePersistField "Token"
