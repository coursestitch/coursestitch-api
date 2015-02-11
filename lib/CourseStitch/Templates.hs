{-# LANGUAGE OverloadedStrings #-}

module CourseStitch.Templates (
    module CourseStitch.Templates.Resource,
    module CourseStitch.Templates.Relationship,
    module CourseStitch.Templates.Concept,
    module CourseStitch.Templates.Topic,
    module CourseStitch.Templates.User,
    template
) where

import CourseStitch.Templates.Resource hiding (relationship)
import CourseStitch.Templates.Relationship
import CourseStitch.Templates.Concept
import CourseStitch.Templates.Topic
import CourseStitch.Templates.User

import Web.Scotty (ActionM, raw, setHeader)
import Lucid (Html, renderBS)

template :: Html () -> ActionM ()
template html = do
    setHeader "Content-Type" "text/html"
    raw . renderBS $ html
