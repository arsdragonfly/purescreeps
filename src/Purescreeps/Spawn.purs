module Purescreeps.Spawn where

import Prelude

import Data.Array (catMaybes, length)
import Data.Either (Either)
import Data.List (elem)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect (Effect)
import Purescreeps.Colony (Colony(..))
import Purescreeps.Creep (BodySpec, genericCreep)
import Screeps.Extension (Extension)
import Screeps.FindType (find_my_spawns, find_my_structures)
import Screeps.ReturnCode (ReturnCode)
import Screeps.Room (find, find')
import Screeps.RoomObject (room)
import Screeps.Spawn (Spawn, createCreep')
import Screeps.Structure (AnyStructure, fromAnyStructure, structureType, structure_extension)

createSpecifiedCreep :: Spawn → BodySpec → Effect (Either ReturnCode String)
createSpecifiedCreep spawn spec = createCreep' spawn spec Nothing { colony: room spawn }

findSpawns :: Colony → Array Spawn
findSpawns (Colony room) = find room find_my_spawns

findExtensions :: Colony → Array Extension
findExtensions (Colony room) =
  find' room find_my_structures
    ( \s →
        elem (structureType s)
          [ structure_extension ]
    )
    # (map (fromAnyStructure :: AnyStructure → Maybe Extension))
    # catMaybes

findCapacity :: Colony → Int
findCapacity colony = 300 + (length (findExtensions colony) * 50) / (length (findSpawns colony))

createCreeps :: Colony → Effect (Array (Either ReturnCode String))
createCreeps colony = sequence $ map (\s → createSpecifiedCreep s (genericCreep $ findCapacity colony)) $ findSpawns colony
