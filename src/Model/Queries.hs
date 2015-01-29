module Model.Queries where

import Data.Int (Int64)

import Data.String (fromString)
import Data.Text (Text)
import Data.List (unzip3, nub)
import Data.Maybe (catMaybes, listToMaybe)

import Database.Persist (Entity, insert, get, entityVal, selectList)
import Database.Persist.Sql (SqlPersistT, toSqlKey)
import Database.Esqueleto

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

-- Select all Users in the database
getUsers :: SqlPersistT IO [Entity User]
getUsers = selectList [] []

-- Get a single user by their unique name
getUser :: Text -> SqlPersistT IO (Maybe (Entity User))
getUser name = getBy $ UniqueName name

-- For authentication purposes, see whether we have a Session active for a User
-- with the given Token.
getUserForToken :: Token -> SqlPersistT IO (Maybe (Entity User))
getUserForToken token = fmap listToMaybe $ select $
    from $ \(user `InnerJoin` session) -> do
    on (user ^. UserId ==. session ^. SessionUser)
    where_ (session ^. SessionToken ==. val token)
    return user
