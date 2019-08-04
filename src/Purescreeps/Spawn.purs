module Purescreeps.Spawn where

import Data.List (List)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect (Effect)
import Prelude (bind, map, ($), (<<<))
import Purescreeps.Colony (Colony(..), findColonies)
import Screeps.BodyPartType (BodyPartType, part_carry, part_move, part_work)
import Screeps.FindType (find_my_spawns)
import Screeps.ReturnCode (ReturnCode)
import Screeps.Room (find)
import Screeps.Spawn (Spawn, createCreep')

type BodySpec = Array BodyPartType

createFixedCreep :: Spawn -> Effect (Either ReturnCode String)
createFixedCreep s = createCreep' s [part_move, part_carry, part_work] Nothing {spawn : s}

findSpawns :: Colony -> Array Spawn
findSpawns (Colony room) = find room find_my_spawns

createCreeps :: Colony -> Effect (Array (Either ReturnCode String))
createCreeps = sequence <<< map createFixedCreep <<< findSpawns

createCreepsForAllColonies :: Effect (List (Array (Either ReturnCode String)))
createCreepsForAllColonies = do
  colonies <- findColonies
  sequence $ map createCreeps colonies