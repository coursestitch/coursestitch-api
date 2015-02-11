{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module CourseStitch where

import Web.Scotty (ScottyM, get, put, post, delete)
import qualified CourseStitch.Handlers as Handlers
import CourseStitch.Models.RunDB (RunDB)

api :: RunDB -> ScottyM ()
api runDB = do
    get "/resource" $ Handlers.resources runDB
    post "/resource" $ Handlers.resourceCreate runDB
    put "/resource/:resource" $ Handlers.resourceUpdate runDB
    delete "/resource/:resource" $ Handlers.resourceDelete runDB

    get "/relationship" $ Handlers.relationships runDB
    get "/relationship/resource/:resource/:relationship/concept/:concept" $ Handlers.relationship runDB
    put "/relationship/resource/:resource/:relationship/concept/:concept" $ Handlers.relationshipCreate runDB
    delete "/relationship/resource/:resource/:relationship/concept/:concept" $ Handlers.relationshipDelete runDB

    get "/concept" $ Handlers.concepts runDB
    post "/concept" $ Handlers.conceptCreate runDB
    put "/concept/:concept" $ Handlers.conceptUpdate runDB
    delete "/concept/:concept" $ Handlers.conceptDelete runDB

    get "/topic" $ Handlers.topics runDB
    post "/topic" $ Handlers.topicCreate runDB
    get "/topic/new" $ Handlers.topicNew runDB
    get "/topic/:topic" $ Handlers.topic runDB

    get "/user" $ Handlers.users runDB
    get "/user/:user" $ Handlers.user runDB
    get "/session/new" $ Handlers.loginForm runDB
    post "/session" $ Handlers.login runDB
    delete "/session" $ Handlers.logout runDB

    get "/mastery" $ Handlers.masteries runDB -- For debugging only! Remove me!
    put "/mastery/resource/:resource" $ Handlers.resourceMasteryCreate runDB
    delete "/mastery/resource/:resource" $ Handlers.resourceMasteryDelete runDB
    put "/mastery/concept/:concept" $ Handlers.conceptMasteryCreate runDB
    delete "/mastery/concept/:concept" $ Handlers.conceptMasteryDelete runDB

