module Model.Queries where

import Data.Int (Int64)

import Data.String (fromString)
import Data.List (unzip3, nub)
import Data.Maybe (catMaybes)

import Database.Persist (Entity, insert, get, entityVal, selectList)
import Database.Persist.Sql (SqlPersistT, toSqlKey)
import Database.Esqueleto (select, from, where_, (^.), (?.), (==.), on, InnerJoin(..), LeftOuterJoin(..), val, just)

import Model

group :: [(a, Maybe b)] -> Maybe (a, [b])
group abs = case as of []   -> Nothing
                       a:as -> Just (a, catMaybes bs)
    where (as, bs) = unzip abs

groups :: Eq a => [(a, b)] -> [(a, [b])]
groups abs = [(a', [b | (a, b) <- abs, a == a']) | a' <- nub as]
    where as = map fst abs

joinMaybe :: (Maybe a, Maybe b) -> Maybe (a, b)
joinMaybe (Nothing, _) = Nothing
joinMaybe (_, Nothing) = Nothing
joinMaybe (Just x, Just y) = Just (x, y)

relationships zs = zs'
    where (xs, rels, ys) = unzip3 zs
          rels' = map joinMaybe $ zip (map (fmap (relationshipRelationship . entityVal)) rels) ys
          zs'   = fmap (\(x, rels) -> (x, groups rels)) $ group (zip xs rels')

-- Select all Resources in the database
getResources :: SqlPersistT IO [Entity Resource]
getResources = selectList [] []

getResource :: Int64 -> SqlPersistT IO (Maybe (Entity Resource, [(RelationshipType, [Entity Concept])]))
getResource id = do
    -- Select resource with given id from the DB, and the concepts associated with it.
    rs <- select $
        from $ \(resource `LeftOuterJoin` relationship `LeftOuterJoin` concept) -> do
        on (just (resource ^. ResourceId) ==. relationship ?. RelationshipResource)
        on (concept ?. ConceptId   ==. relationship ?. RelationshipConcept)
        where_ (resource ^. ResourceId ==. (val . toSqlKey) id)
        return (resource, relationship, concept)
    
    -- Group together the resources into a list
    return $ relationships rs


-- Select all Concepts in the database
getConcepts :: SqlPersistT IO [Entity Concept]
getConcepts = selectList [] []

getConcept :: String -> SqlPersistT IO (Maybe (Entity Concept, [(RelationshipType, [Entity Resource])]))
getConcept title = do
    -- Select concept with given title from the DB, and the resources associated with it.
    cs <- select $
        from $ \(concept `LeftOuterJoin` relationship `LeftOuterJoin` resource) -> do
        on (just (concept ^. ConceptId) ==. relationship ?. RelationshipConcept)
        on (resource ?. ResourceId   ==. relationship ?. RelationshipResource)
        where_ (concept ^. ConceptTitle ==. (val . fromString) title)
        return (concept, relationship, resource)
    
    -- Group together the concepts into a list
    return $ relationships cs


-- Select all Topics in the database
getTopics :: SqlPersistT IO [Entity Topic]
getTopics = selectList [] []

getTopic :: String -> SqlPersistT IO (Maybe (Entity Topic, [Entity Concept]))
getTopic title = do
    -- Select topic with given title from the DB, and the concepts associated with them.
    tcs <- select $
        from $ \(topic `LeftOuterJoin` concept) -> do
        on (just (just (topic ^. TopicId)) ==. concept ?. ConceptTopic)
        where_ (topic ^. TopicTitle ==. (val . fromString) title)
        return (topic, concept)

    -- Group together the concepts into a list
    return $ group tcs
