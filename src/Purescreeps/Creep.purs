module Purescreeps.Creep where

import Prelude

import Data.Array (concat)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Purescreeps.ReturnCode (Status, toStatus, orElse)
import Screeps.BodyPartType (BodyPartType, part_carry, part_move, part_work)
import Screeps.Creep (Creep, moveOpts, moveTo')
import Screeps.ReturnCode (ReturnCode)
import Screeps.RoomObject (class RoomObject, AnyRoomObject)
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

type Job = Creep → Effect Status

toTargetToJob :: forall a. RoomObject a ⇒ (Creep → a → Effect ReturnCode) → (a → Job)
toTargetToJob f = (\target creep → (liftM1 toStatus) (f creep target))

moveToAction :: forall a. RoomObject a ⇒ (a → Job) → (a → Job)
moveToAction action =
  ( \target creep →
      action target creep
        >>= orElse
            ( 
              toTargetToJob (\c t → (moveTo' c (TargetObj t) (moveOpts { visualizePathStyle = Just {} }))) target creep
            )
  )

type Target = Tuple (AnyRoomObject → Job) AnyRoomObject