module Purescreeps.Harvest where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Purescreeps.Random (sampleByStringHash)
import Screeps.Creep (Creep, name)
import Screeps.FindType (find_sources)
import Screeps.Room (find)
import Screeps.RoomObject (room)
import Screeps.Source (Source)

findCorrespondingSource :: Creep → Maybe Source
findCorrespondingSource creep = sampleByStringHash (name creep) $ find (room creep) find_sources