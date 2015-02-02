module Template.Topic where

import Lucid
import Model
import Database.Persist (Entity)

topicSimple :: Entity Topic -> Html ()
