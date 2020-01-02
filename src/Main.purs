module Main where

import Prelude

import Data.Array (zipWith)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (logShow)
import Purescreeps.Colony (findColonies)
import Purescreeps.Creep (moveToAction, toTargetToJob, Job, Target)
import Purescreeps.Harvest (findCorrespondingSource)
import Purescreeps.ReturnCode (Status, orElse)
import Purescreeps.Spawn (createCreeps, createCreepsForAllColonies)
import Screeps.Controller (Controller)
import Screeps.Creep (Creep, harvestSource, transferToStructure, upgradeController)
import Screeps.Game (creeps, rooms) as Game
import Screeps.Path (target)
import Screeps.Resource (resource_energy)
import Screeps.Room (controller)
import Screeps.RoomObject (room)
import Screeps.Source (Source)
import Screeps.Stores (storeTotalFree, storeTotalUsed)
import Screeps.Structure (class Structure)

main :: Effect Unit
main = do
  colonies <- findColonies
  (sequence $ map createCreeps colonies) >>= logShow
  creeps <- Game.creeps
  (traverse runCreep creeps) >>= logShow

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

runTarget :: Target -> Job
runTarget target =
  ( \creep → case { source: findCorrespondingSource creep } of
      { source: Just source } →
        ( if storeTotalUsed creep == 0 then
            (moveToAction harvestSource' source creep)
          else
            if storeTotalFree creep == 0 then
              (moveToAction (fst target) (snd target) creep)
            else
              (harvestSource' source creep >>= orElse (moveToAction (fst target) (snd target) creep))
        )
      { source: _ } → pure $ Left "No source/controller found"
  )

harvestSource' :: Source → Job
harvestSource' = toTargetToJob harvestSource

upgradeController' :: Controller → Job
upgradeController' = toTargetToJob upgradeController

transferEnergyToStructure' :: ∀ s. Structure s ⇒ s → Job
transferEnergyToStructure' = toTargetToJob (\c t → transferToStructure c t resource_energy)

assignTargets :: Array Target → Array (Tuple String Creep) → Effect (Array (Tuple String Status))
assignTargets targets creeps = (traverse sequence) (zipWith (\target creep → runTarget target <$> creep) targets creeps)
