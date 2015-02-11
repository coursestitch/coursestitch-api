{-# LANGUAGE OverloadedStrings #-}

module Templates.Concept where

import Data.String (fromString)
import Data.Text.Encoding (decodeUtf8)
import Data.Monoid (mappend, mconcat)

import Network.HTTP.Types.Method (methodGet, methodPost, methodPut, methodDelete)

import Lucid
import CourseStitch.Models
import Database.Persist (Entity, entityVal)
import Database.Persist.Sql (unSqlBackendKey)

import CourseStitch.Templates.Concept
import CourseStitch.Templates.Templates
import Templates.Website (page)

conceptDetailed :: Entity Concept -> Maybe (Entity Topic) -> [(RelationshipType, [Entity Resource])] -> Html ()
conceptDetailed concept topic rels = do
    conceptLink concept $ conceptHeading concept
    conceptTopicArticle topic
    mconcat $ map (uncurry conceptResources) rels

conceptForm :: Maybe (Entity Concept) -> Html ()
conceptForm concept = do
    form_ [action_ uri, method_ method] $ do
        fieldset_ $ do
            input "Title" "title" $ get conceptTitle
            input "Topic" "topic" $ getConceptTopicId
        input_ [type_ "submit"]

    script_ [src_ "/js/form-methods.js"] ("" :: String)

    where get f = fmap (f . entityVal) concept
          getConceptTopicId = fmap (fromString . show . unSqlBackendKey . unTopicKey) getConceptTopic
          getConceptTopic = do
            c <- concept
            (conceptTopic . entityVal) c
          uri = case concept of
                Just concept -> conceptUri concept
                Nothing       -> "/concept"
          method = case concept of
                Just _  -> decodeUtf8 methodPut
                Nothing -> decodeUtf8 methodPost

conceptPage :: Entity Concept -> Maybe (Entity Topic) -> [(RelationshipType, [Entity Resource])] -> Html()
conceptPage c t rs = page $ conceptDetailed c t rs
