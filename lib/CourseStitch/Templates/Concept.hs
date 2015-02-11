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

import CourseStitch.Templates.Utils
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
