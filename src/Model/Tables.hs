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
import Database.Persist
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
    UniqueUrl url
    course   Text
    summary  Text
    preview  Text
    keywords Text
    deriving Show

Concept
    topic TopicId Maybe
    title Text
    deriving Show

Topic
    title   Text
    summary Text
    deriving Show

Relationship
    resource     ResourceId
    relationship RelationshipType
    concept      ConceptId
    UniqueResourceConcept resource concept
    deriving Show

User
    name Text
    UniqueName name
    hash Text
    salt Text
    deriving Show

Session
    user  UserId
    token Token
    UniqueUserToken user token
    deriving Show
|]
