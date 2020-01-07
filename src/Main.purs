module Main where

import Prelude

import Data.Map (toUnfoldable)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Console (logShow)
import Purescreeps.Colony (findColonies)
import Purescreeps.Context (defaultContext, displayFootprintForCurrentTick)
import Purescreeps.Creep (clearDeadCreepMemory, assignTargets, generateTargets)
import Purescreeps.Spawn (createCreeps)
import Screeps.Game (creeps) as Game
import Screeps.Memory (getMemoryGlobal, set)

main :: Effect Unit
main = do
  clearDeadCreepMemory
  memory ← getMemoryGlobal
  set memory "context" defaultContext
  colonies ← findColonies
  (sequence $ map createCreeps colonies) >>= logShow
  void $ displayFootprintForCurrentTick colonies
  creeps ← Game.creeps
  (assignTargets (generateTargets colonies creeps) (toUnfoldable creeps)) >>= logShow
