{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.Tables where

import Data.Text (Text)
import Data.ByteString.Char8 (ByteString)

import Database.Persist.TH

import Model.Types

{- See the following page in the Yesod book for a description of what these
 - quasiquotes generate.
 - http://www.yesodweb.com/book/persistent#persistent_code_generation -}

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Resource
    title    Text
    media    Text
    url      Text
    UniqueResourceUrl url
    course   Text
    summary  Text
    preview  Text
    keywords Text
    deriving Show

Concept
    topic TopicId Maybe
    title Text
    UniqueConceptTitle title
    deriving Show

Topic
    title   Text
    summary Text
    UniqueTopicTitle title
    deriving Show

Relationship
    resource     ResourceId
    relationship RelationshipType
    concept      ConceptId
    UniqueResourceConcept resource concept
    deriving Show

User
    name Text
    UniqueUserName name
    hash ByteString
    deriving Show

Session
    user  UserId
    token Token
    deriving Show
|]
