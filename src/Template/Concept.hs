{-# LANGUAGE OverloadedStrings #-}

module Template.Concept where

import Data.String (fromString)
import Data.Text.Encoding (decodeUtf8)
import Data.Monoid (mappend, mconcat)

import Network.HTTP.Types.Method (methodGet, methodPost, methodPut, methodDelete)

import Lucid
import Model
import Database.Persist (Entity, entityVal)

import Template.Template
import  {-# SOURCE #-} Template.Resource (resourceSimple)

concepts :: [Entity Concept] -> Html ()
concepts cs = unorderedList $ map conceptSimple cs

concept :: Entity Concept  -> [(RelationshipType, [Entity Resource])] -> Html ()
concept concept resources = article_ $ conceptDetailed concept resources

conceptCreated :: Entity Concept -> [(RelationshipType, [Entity Resource])] -> Html ()
conceptCreated r cs = do
    p_ $ toHtml $ mconcat [conceptUri r, " was created successfully"]
    concept r cs

conceptUpdated :: Entity Concept -> [(RelationshipType, [Entity Resource])] -> Html ()
conceptUpdated r cs = do
    p_ $ toHtml $ mconcat [conceptUri r, " was updated successfully"]
    concept r cs

conceptDeleted :: Entity Concept -> [(RelationshipType, [Entity Resource])] -> Html ()
conceptDeleted r cs = do
    p_ $ toHtml $ mconcat [conceptUri r, " was deleted"]
    concept r cs

conceptSimple :: Entity Concept -> Html ()
conceptSimple concept = do
    conceptLink concept $ conceptHeading concept

conceptForm :: Maybe (Entity Concept) -> Html ()
conceptForm concept = do
    form_ [action_ uri, method_ method] $ do
        fieldset_ $ do
            input "Title" "title" $ get conceptTitle
        input_ [type_ "submit"]

    script_ [src_ "/js/form-methods.js"] ("" :: String)

    where get f = fmap (f . entityVal) concept
          uri = case concept of
                Just concept -> conceptUri concept
                Nothing       -> "/concept"
          method = case concept of
                Just _  -> decodeUtf8 methodPut
                Nothing -> decodeUtf8 methodPost

conceptDetailed :: Entity Concept -> [(RelationshipType, [Entity Resource])] -> Html ()
conceptDetailed concept rels = do
    conceptLink concept $ conceptHeading concept
    mconcat $ map (uncurry conceptResources) rels

conceptResources :: RelationshipType -> [Entity Resource] -> Html ()
conceptResources rel resources = do
    conceptResourcesHeading rel
    case resources of
        [] -> conceptResourcesMissing rel
        resources -> unorderedList $ map resourceSimple resources


conceptUri concept = mappend "/concept/" ((conceptTitle . entityVal) concept)
conceptLink concept html = link (conceptUri concept) html

conceptHeading = h1_ . toHtml . conceptTitle . entityVal

conceptResourcesHeading rel = h2_ ((fromString . show) rel `mappend` " by")
conceptResourcesMissing rel = p_ ("There are no resources that " `mappend` (fromString . show) rel `mappend` " this concept")
