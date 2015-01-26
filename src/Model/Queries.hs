module Model.Queries where

import Data.String (fromString)
import Data.Maybe (fromJust, isJust)

import Database.Persist (Entity, insert, get, entityVal)
import Database.Persist.Sql (SqlPersistT)
import Database.Esqueleto (select, from, where_, (^.), (?.), (==.), on, InnerJoin(..), LeftOuterJoin(..), val, just)

import Model

group :: [(a, Maybe b)] -> Maybe (a, [b])
group abs = case as of []   -> Nothing
                       a:as -> Just (a, justBs)
    where (as, bs) = unzip abs
          justBs = map fromJust $ filter isJust bs


getConcepts :: SqlPersistT IO [Concept]
getConcepts = do
    -- Select all concepts in the DB
    topics <- select $
        from $ \concept -> do
        return concept
    -- entityVal upwraps database entities.
    return $ map entityVal topics


getTopics :: SqlPersistT IO [Topic]
getTopics = do
    -- Select all topics in the DB, and the concepts associated with them.
    topics <- select $
        from $ \(topic `LeftOuterJoin` concept) -> do
        on (concept ^. ConceptTopic ==. just (topic ^. TopicId))
        return topic
    -- entityVal upwraps database entities.
    return $ map entityVal topics

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
