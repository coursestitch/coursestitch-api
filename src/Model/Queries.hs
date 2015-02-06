module Model.Queries where

import Data.Int (Int64)

import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.List (unzip3, nub)
import Data.Maybe (catMaybes, listToMaybe)
import System.Entropy (getEntropy)
import Data.ByteString.Base64 (encode)
import Control.Monad.IO.Class (liftIO)

import qualified Database.Persist as P
import Database.Persist (Entity, insertUnique, insertBy, get, getByValue, entityVal, selectFirst, selectList, deleteWhere)
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

-- Create a resource
newResource :: Resource -> SqlPersistT IO (Maybe (Entity Resource))
newResource resource = do
    key <- insertUnique resource
    return $ case key of
        Just key -> Just $ Entity key resource
        Nothing  -> Nothing

-- Update a resource
editResource :: Int64 -> Resource -> SqlPersistT IO Resource
editResource id resource = do
    replace (toSqlKey id) resource
    return resource

-- Delete a resource
deleteResource :: Int64 -> SqlPersistT IO ()
deleteResource id = deleteWhere [ResourceId P.==. toSqlKey id]


-- Select all Relationships in the database
getRelationships :: SqlPersistT IO [Entity Relationship]
getRelationships = selectList [] []

-- Select relationship from the DB, and the concept and resource associated with it.
getRelationship :: Relationship -> SqlPersistT IO (Maybe (Entity Relationship, Entity Resource, Entity Concept))
getRelationship rel = do
    let resourceKey = relationshipResource rel
    let conceptKey  = relationshipConcept rel

    rel' <- getByValue rel
    resource <- get resourceKey
    concept <- get conceptKey

    return $ case (rel', resource, concept) of
        (Just rel, Just resource, Just concept) -> Just (rel, Entity resourceKey resource, Entity conceptKey concept)
        otherwise -> Nothing

-- Create a relationship
newRelationship :: Relationship -> SqlPersistT IO (Entity Relationship)
newRelationship relationship = do
    insertion <- insertBy relationship
    return $ case insertion of
        Left entity -> entity
        Right key   -> Entity key relationship

-- Delete a relationship
deleteRelationship :: Entity Relationship -> SqlPersistT IO ()
deleteRelationship rel = P.delete $ entityKey rel


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

-- Create a concept
newConcept :: Concept -> SqlPersistT IO (Maybe (Entity Concept))
newConcept concept = do
    key <- insertUnique concept
    return $ case key of
        Just key -> Just $ Entity key concept
        Nothing  -> Nothing

-- Update a concept
editConcept :: String -> Concept -> SqlPersistT IO Concept
editConcept name concept = do
    oldConcept <- selectFirst [ConceptTitle P.==. fromString name] []
    case fmap entityKey oldConcept of
        Nothing  -> return ()
        Just key -> replace key concept
    return concept

-- Delete a concept
deleteConcept :: String -> SqlPersistT IO ()
deleteConcept name = deleteWhere [ConceptTitle P.==. fromString name]


-- Select all Topics in the database
getTopics :: SqlPersistT IO [Entity Topic]
getTopics = selectList [] []

-- Select all Topics in the database
getTopicsFromKeywords :: [String] -> SqlPersistT IO [(Entity Topic, [Entity Concept])]
getTopicsFromKeywords kws = do
    topics <- select $
        from $ \(topic `InnerJoin` concept) -> do
        on (just (topic ^. TopicId) ==. concept ^. ConceptTopic)
        where_ (foldl1 (||.) [topic ^. TopicTitle ==. (val . fromString) kw | kw <- kws])
        return (topic, concept)

    -- Group together the topics into a list
    return $ groups topics

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

-- Get the topic entity from the database
getConceptTopic :: Entity Concept -> SqlPersistT IO (Maybe (Entity Topic))
getConceptTopic concept = do
    topic <- case (conceptTopic . entityVal) concept of
        Nothing      -> return Nothing
        Just topicId -> selectFirst [TopicId P.==. topicId] []
    return topic


-- Select all Users in the database
getUsers :: SqlPersistT IO [Entity User]
getUsers = selectList [] []

-- Get a single user by their unique name
getUser :: Text -> SqlPersistT IO (Maybe (Entity User))
getUser name = getBy $ UniqueUserName name

-- For authentication purposes, see whether we have a Session active for a User
-- with the given Token.
getUserForToken :: Token -> SqlPersistT IO (Maybe (Entity User))
getUserForToken token = fmap listToMaybe $ select $
    from $ \(user `InnerJoin` session) -> do
    on (user ^. UserId ==. session ^. SessionUser)
    where_ (session ^. SessionToken ==. val token)
    return user

getSession :: Token -> SqlPersistT IO (Maybe (Entity Session))
getSession token = fmap listToMaybe $ selectList [SessionToken P.==. token] []

newToken :: IO Token
newToken = fmap (Token . decodeUtf8 . encode) (getEntropy 32)

newSession :: Entity User -> SqlPersistT IO Token
newSession u = do
    token <- liftIO newToken
    insert_ $ Session (entityKey u) token
    return token

deleteSessions :: Token -> SqlPersistT IO ()
deleteSessions token = deleteWhere [SessionToken P.==. token]

getMasteries :: SqlPersistT IO ([Entity ResourceMastery], [Entity ConceptMastery])
getMasteries = do
    rm <- selectList [] []
    cm <- selectList [] []
    return (rm, cm)

newResourceMastery :: Entity User -> Entity Resource -> SqlPersistT IO (Maybe (Entity ResourceMastery))
newResourceMastery user resource = do
    maybeMasteryKey <- insertUnique mastery
    case maybeMasteryKey of
        Nothing  -> return Nothing
        Just key -> return $ Just (Entity key mastery)
    where mastery = ResourceMastery (entityKey user) (entityKey resource)

deleteResourceMastery :: Entity User -> Entity Resource -> SqlPersistT IO ()
deleteResourceMastery user resource = deleteWhere
    [ResourceMasteryUser P.==. entityKey user, ResourceMasteryResource P.==. entityKey resource]

newConceptMastery :: Entity User -> Entity Concept -> SqlPersistT IO (Maybe (Entity ConceptMastery))
newConceptMastery user concept = do
    maybeMasteryKey <- insertUnique mastery
    case maybeMasteryKey of
        Nothing  -> return Nothing
        Just key -> return $ Just (Entity key mastery)
    where mastery = ConceptMastery (entityKey user) (entityKey concept)

deleteConceptMastery :: Entity User -> Entity Concept -> SqlPersistT IO ()
deleteConceptMastery user concept = deleteWhere
    [ConceptMasteryUser P.==. entityKey user, ConceptMasteryConcept P.==. entityKey concept]
