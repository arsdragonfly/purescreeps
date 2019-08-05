module Purescreeps.Creep where

import Prelude

import Data.Array (concat)
import Screeps.BodyPartType (BodyPartType, part_carry, part_move, part_work)
import Screeps.Spawn (Spawn)

type CreepMemory = { spawn :: Spawn }

type BodySpec = Array BodyPartType

genericCreep :: Int -> BodySpec
genericCreep capacity | capacity < 50 = []
genericCreep capacity | capacity < 100 = [part_move]
genericCreep capacity | capacity < 150 = [part_move, part_carry]
genericCreep capacity | capacity < 250 = [part_move, part_carry, part_work]
genericCreep capacity = concat [[part_move, part_carry, part_work], genericCreep (capacity - 200)]