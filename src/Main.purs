module Main where

import Prelude

import Data.Array (zipWith)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (logShow)
import Purescreeps.Creep (moveToAction, toTargetToJob, Job, Jobs)
import Purescreeps.Harvest (findCorrespondingSource)
import Purescreeps.ReturnCode (Status, orElse)
import Purescreeps.Spawn (createCreepsForAllColonies)
import Screeps.Controller (Controller)
import Screeps.Creep (Creep, harvestSource, transferToStructure, upgradeController)
import Screeps.Game (creeps)
import Screeps.Resource (resource_energy)
import Screeps.Room (controller)
import Screeps.RoomObject (room)
import Screeps.Source (Source)
import Screeps.Stores (storeTotalFree, storeTotalUsed)
import Screeps.Structure (class Structure)

main :: Effect Unit
main = do
  createCreepsForAllColonies >>= logShow
  creeps >>= traverse runCreep >>= logShow

runCreep :: Job
runCreep creep = case { source: findCorrespondingSource creep
  , controller: controller (room creep)
  } of
  { source: Just source, controller: Just controller } →
    ( if storeTotalUsed creep == 0 then
        (moveToAction harvestSource' source creep)
      else
        if storeTotalFree creep == 0 then
          (moveToAction upgradeController' controller creep)
        else
          (harvestSource' source creep >>= orElse (moveToAction upgradeController' controller creep))
    )
  { source: _, controller: _ } → pure $ Left "No source/controller found"

harvestSource' :: Source → Job
harvestSource' = toTargetToJob harvestSource

upgradeController' :: Controller → Job
upgradeController' = toTargetToJob upgradeController

transferEnergyToStructure' :: ∀ s. Structure s ⇒ s → Job
transferEnergyToStructure' = toTargetToJob (\c t → transferToStructure c t resource_energy)

assignJobs :: Jobs → Array (Tuple String Creep) → Effect (Array (Tuple String Status))
assignJobs jobs creeps = (traverse sequence) (zipWith (<$>) jobs creeps)
