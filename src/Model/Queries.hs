module Model.Queries where

import Data.String (fromString)

import Database.Persist (Entity, insert, get, entityVal)
import Database.Persist.Sql (SqlPersistT)
import Database.Esqueleto (select, from, where_, (^.), (?.), (==.), on, InnerJoin(..), LeftOuterJoin(..), val, just)

import Model

getTopics :: SqlPersistT IO [Topic]
getTopics = do
    -- Select all topics in the DB, and the concepts associated with them.
    topics <- select $
        from $ \(topic `LeftOuterJoin` concept) -> do
        on (concept ^. ConceptTopic ==. just (topic ^. TopicId))
        return topic
    -- entityVal upwraps database entities.
    return $ map entityVal topics

getTopic :: String -> SqlPersistT IO (Maybe Topic)
getTopic title = do
    -- Select topic with given title from the DB, and the concepts associated with them.
    topics <- select $
        from $ \(topic `LeftOuterJoin` concept) -> do
        on (concept ^. ConceptTopic ==. just (topic ^. TopicId))
        where_ (topic ^. TopicTitle ==. (val . fromString) title)
        return topic
    
    -- Determine if we have a single unique topic
    let topic = case topics of []   -> Nothing
                               t:[] -> Just $ entityVal t
                               t:ts -> Nothing

    return $ topic
