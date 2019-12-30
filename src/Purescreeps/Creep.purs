module Purescreeps.Creep where

import Prelude
import Data.Array (concat)
import Screeps.BodyPartType (BodyPartType, part_carry, part_move, part_work)
import Screeps.Spawn (Spawn)

type CreepMemory
  = { spawn :: Spawn }

type BodySpec
  = Array BodyPartType

genericCreep :: Int → BodySpec
genericCreep capacity
  | capacity < 50 = []

genericCreep capacity
  | capacity < 100 = [ part_move ]

genericCreep capacity
  | capacity < 150 = [ part_carry, part_move ]

genericCreep capacity
  | capacity < 250 = [ part_work, part_carry, part_move ]

genericCreep capacity = concat [ genericCreep (capacity - 200), [ part_work, part_carry, part_move ] ]
