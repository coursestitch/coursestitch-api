{-# LANGUAGE OverloadedStrings #-}

module CourseStitch.Templates.Concept where

import Data.String (fromString)
import Data.Text.Encoding (decodeUtf8)
import Data.Monoid (mappend, mconcat)

import Network.HTTP.Types.Method (methodGet, methodPost, methodPut, methodDelete)

import Lucid
import CourseStitch.Models
import Database.Persist (Entity, entityVal)
import Database.Persist.Sql (unSqlBackendKey)

import CourseStitch.Templates.Templates
import {-# SOURCE #-} CourseStitch.Templates.Topic (topicSimple)
import {-# SOURCE #-} CourseStitch.Templates.Resource (resourceSimple)

concepts :: [Entity Concept] -> Html ()
concepts cs = unorderedList $ map conceptSimple cs

concept :: Entity Concept -> Html ()
concept concept = article_ $ conceptSimple concept

conceptCreated :: Entity Concept -> Html ()
conceptCreated c = do
    p_ $ toHtml $ mconcat [conceptUri c, " was created successfully"]
    concept c

conceptUpdated :: Entity Concept -> Html ()
conceptUpdated c = do
    p_ $ toHtml $ mconcat [conceptUri c, " was updated successfully"]
    concept c

conceptDeleted :: Entity Concept -> Html ()
conceptDeleted c = do
    p_ $ toHtml $ mconcat [conceptUri c, " was deleted"]
    concept c

conceptSimple :: Entity Concept -> Html ()
conceptSimple concept = do
    conceptLink concept $ conceptHeading concept

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

conceptDetailed :: Entity Concept -> Maybe (Entity Topic) -> [(RelationshipType, [Entity Resource])] -> Html ()
conceptDetailed concept topic rels = do
    conceptLink concept $ conceptHeading concept
    conceptTopicArticle topic
    mconcat $ map (uncurry conceptResources) rels

conceptTopicArticle :: Maybe (Entity Topic) -> Html ()
conceptTopicArticle topic = article_ $ do
    case topic of
        Nothing    -> conceptTopicMissing
        Just topic -> topicSimple topic

conceptResources :: RelationshipType -> [Entity Resource] -> Html ()
conceptResources rel resources = do
    conceptResourcesHeading rel
    case resources of
        [] -> conceptResourcesMissing rel
        resources -> unorderedList $ map resourceSimple resources


conceptUri concept = mappend "/concept/" ((conceptTitle . entityVal) concept)
conceptLink concept html = link (conceptUri concept) html

conceptHeading = h1_ . toHtml . conceptTitle . entityVal

conceptTopicMissing = p_ "This concept has no topic"

conceptResourcesHeading rel = h2_ ((fromString . show) rel `mappend` " by")
conceptResourcesMissing rel = p_ ("There are no resources that " `mappend` (fromString . show) rel `mappend` " this concept")
