module Model.Queries where

import Database.Persist (Entity, insert, get, entityVal)
import Database.Persist.Sql (SqlPersistT)
import Database.Esqueleto (select, from, (^.), (?.), (==.), on, InnerJoin(..), LeftOuterJoin(..), just)

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


