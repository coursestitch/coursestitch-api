module CourseStitch.Templates.Resource where

import Lucid
import CourseStitch.Models
import Database.Persist (Entity)

resourceSimple :: Entity Resource -> Html ()
