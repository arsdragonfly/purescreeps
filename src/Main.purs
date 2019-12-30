module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log, logShow)
import Purescreeps.Harvest (findCorrespondingSource)
import Purescreeps.ReturnCode (orElse)
import Purescreeps.Spawn (createCreepsForAllColonies)
import Screeps.Creep (Creep, harvestSource, moveOpts, moveTo', say)
import Screeps.Game (creeps)
import Screeps.Resource (resource_energy)
import Screeps.Stores (heldResources, store, storeCapacity, storeTotalUsed, storeFree)
import Screeps.Types (TargetPosition(..))

main :: Effect Unit
main = do
  createCreepsForAllColonies >>= logShow
  void $ creeps >>= traverse runCreep
  void $ creeps >>= traverse sayCapacity

runCreep :: Creep → Effect Unit
runCreep creep = case findCorrespondingSource creep of
    Just source →
      ( harvestSource creep source
          >>= orElse (moveTo' creep (TargetObj source) (moveOpts { visualizePathStyle = Just {} }))
          >>= logShow
      )
    Nothing → log "No source found"

sayCapacity :: Creep → Effect Unit
sayCapacity creep = void $ say creep (show $ storeFree creep resource_energy)