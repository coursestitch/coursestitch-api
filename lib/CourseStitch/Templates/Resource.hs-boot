module CourseStitch.Templates.Resource where

import Lucid
import CourseStitch.Models
import Database.Persist (Entity)

resourceSimple :: Monad m => Entity Resource -> HtmlT m ()
