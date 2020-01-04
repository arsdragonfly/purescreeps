module Main where

import Prelude
import Data.Map (toUnfoldable)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Console (logShow)
import Purescreeps.Colony (findColonies)
import Purescreeps.Creep (clearDeadCreepMemory, assignTargets, generateTargets)
import Purescreeps.Spawn (createCreeps)
import Screeps.Game (creeps) as Game

main :: Effect Unit
main = do
  clearDeadCreepMemory
  colonies <- findColonies
  (sequence $ map createCreeps colonies) >>= logShow
  creeps <- Game.creeps
  (assignTargets (generateTargets colonies creeps) (toUnfoldable creeps)) >>= logShow
