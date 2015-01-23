module Model.Queries where

import Database.Persist (Entity, insert, get, entityVal)
import Database.Persist.Sql (SqlPersistT)
import Database.Esqueleto (select, from, (^.), (?.), (==.), on, InnerJoin(..), just)

import Model

getTopics :: SqlPersistT IO [Topic]
getTopics = do
    -- Select all concepts in the DB, and get the topic associated with them.
    topics <- select $
        from $ \(concept `InnerJoin` topic) -> do
        on (concept ^. ConceptTopic ==. just (topic ^. TopicId))
        return topic
    -- entityVal upwraps database entities.
    return $ map entityVal topics


