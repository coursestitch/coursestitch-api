module Model.Queries where

import Data.Int (Int64)

import Data.String (fromString)
import Data.Maybe (catMaybes)

import Database.Persist (Entity, insert, get, entityVal, selectList)
import Database.Persist.Sql (SqlPersistT, toSqlKey)
import Database.Esqueleto (select, from, where_, (^.), (?.), (==.), on, InnerJoin(..), LeftOuterJoin(..), val, just)

import Model

group :: [(a, Maybe b)] -> Maybe (a, [b])
group abs = case as of []   -> Nothing
                       a:as -> Just (a, catMaybes bs)
    where (as, bs) = unzip abs


-- Select all Resources in the database
getResources :: SqlPersistT IO [Resource]
getResources = map entityVal `fmap` selectList [] []

getResource :: Int64 -> SqlPersistT IO (Maybe Resource)
getResource id = get $ toSqlKey id


-- Select all Concepts in the database
getConcepts :: SqlPersistT IO [Concept]
getConcepts = map entityVal `fmap` selectList [] []

getConcept :: String -> SqlPersistT IO (Maybe Concept)
getConcept title = do
    -- Select all concepts in the DB
    concepts <- select $
        from $ \concept -> do
        where_ (concept ^. ConceptTitle ==. (val . fromString) title)
        return concept

    -- Get the concept
    return $ case concepts of
        []        -> Nothing
        concept:_ -> Just (entityVal concept)


-- Select all Topics in the database
getTopics :: SqlPersistT IO [Topic]
getTopics = map entityVal `fmap` selectList [] []

getTopic :: String -> SqlPersistT IO (Maybe (Topic, [Concept]))
getTopic title = do
    -- Select topic with given title from the DB, and the concepts associated with them.
    tcs <- select $
        from $ \(topic `LeftOuterJoin` concept) -> do
        on (just (just (topic ^. TopicId)) ==. concept ?. ConceptTopic)
        where_ (topic ^. TopicTitle ==. (val . fromString) title)
        return (topic, concept)

    -- Group together the concepts into a list
    return $ case group tcs of Nothing      -> Nothing
                               Just (t, cs) -> Just (entityVal t, map entityVal cs)
