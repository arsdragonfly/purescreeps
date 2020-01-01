module Purescreeps.Creep where

import Prelude
import Data.Array (concat)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Purescreeps.ReturnCode (Status, toStatus, orElse)
import Screeps.BodyPartType (BodyPartType, part_carry, part_move, part_work)
import Screeps.Creep (Creep, moveOpts, moveTo')
import Screeps.ReturnCode (ReturnCode)
import Screeps.Spawn (Spawn)
import Screeps.Types (TargetPosition(..))

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

toStatusAction :: forall a. (Creep → a → Effect ReturnCode) → (Creep → a → Effect Status)
toStatusAction f = (\creep target → (liftM1 toStatus) (f creep target))

moveToAction :: forall a. (Creep → a → Effect Status) → (Creep → a → Effect Status)
moveToAction action =
  ( \creep target →
      action creep target
        >>= orElse
            ( 
              toStatusAction (\c t → (moveTo' c (TargetObj t) (moveOpts { visualizePathStyle = Just {} }))) creep target
            )
  )
