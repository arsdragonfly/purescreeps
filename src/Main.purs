module Main where

import Prelude

import Data.Map (toUnfoldable)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Console (logShow)
import Purescreeps.Colony (findColonies)
import Purescreeps.Context (defaultContext, getContext, setContext, updateContext, displayFootprintFromContext)
import Purescreeps.Creep (clearDeadCreepMemory, assignTargets, generateTargets)
import Purescreeps.Spawn (createCreeps)
import Purescreeps.Tower (runTowers)
import Screeps.Game (creeps) as Game

main :: Effect Unit
main = do
  clearDeadCreepMemory
  -- context ← getContext
  -- newContext ← updateContext context
  -- _ ←setContext newContext
  colonies ← findColonies
  -- (sequence $ map createCreeps colonies) >>= logShow
  _ ← sequence $ map createCreeps colonies
  -- (sequence $ map runTowers colonies) >>= logShow
  _ ← sequence $ map runTowers colonies
  creeps ← Game.creeps
  _ ← assignTargets (generateTargets colonies creeps) (toUnfoldable creeps)
  mempty