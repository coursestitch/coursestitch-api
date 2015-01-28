module Template.Resource where

import Lucid
import Model
import Database.Persist (Entity)

resourceSimple :: Entity Resource -> Html ()
