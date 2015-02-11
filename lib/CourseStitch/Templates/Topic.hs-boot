module CourseStitch.Templates.Topic where

import Lucid
import CourseStitch.Models
import Database.Persist (Entity)

topicSimple :: Entity Topic -> Html ()
