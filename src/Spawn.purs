module Spawn where

import Screeps.BodyPartType

import Data.Either (Either)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, sequence)
import Effect (Effect)
import Prelude (map, ($), (<$>), (<<<), (>>=))
import Screeps.FindType (find_my_spawns)
import Screeps.Game as Game
import Screeps.ReturnCode (ReturnCode)
import Screeps.Room (find)
import Screeps.Spawn (Spawn, createCreep')

type BodySpec = Array BodyPartType

type CreepMemory = { spawn :: Spawn }

createFixedCreep :: Spawn -> Effect (Either ReturnCode String)
createFixedCreep s = createCreep' s [part_move, part_carry, part_work] Nothing {spawn : s}

findSpawns :: Effect (Map String (Array Spawn))
findSpawns = map (\r -> find r find_my_spawns) <$> Game.rooms

createCreeps :: Effect (Map String (Array (Either ReturnCode String)))
createCreeps = findSpawns >>= (traverse $ sequence <<< map createFixedCreep)