module Purescreeps.Spawn where

import Prelude

import Data.Either (Either)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect (Effect)
import Purescreeps.Colony (Colony(..), findColonies)
import Purescreeps.Creep (BodySpec, genericCreep)
import Screeps.FindType (find_my_spawns)
import Screeps.ReturnCode (ReturnCode)
import Screeps.Room (find)
import Screeps.RoomObject (room)
import Screeps.Spawn (Spawn, createCreep')

createSpecifiedCreep :: Spawn → BodySpec → Effect (Either ReturnCode String)
createSpecifiedCreep spawn spec = createCreep' spawn spec Nothing { colony: room spawn }

findSpawns :: Colony → Array Spawn
findSpawns (Colony room) = find room find_my_spawns

-- TODO: take into account containers
findCapacity :: Colony → Int
findCapacity _ = 300

createCreeps :: Colony → Effect (Array (Either ReturnCode String))
createCreeps colony = sequence $ map (\s → createSpecifiedCreep s (genericCreep $ findCapacity colony)) $ findSpawns colony

createCreepsForAllColonies :: Effect (List (Array (Either ReturnCode String)))
createCreepsForAllColonies = do
  colonies <- findColonies
  sequence $ map createCreeps colonies
