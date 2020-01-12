module Purescreeps.Creep where

import Prelude

import Data.Array (catMaybes, concat, foldMap, replicate, zipWith)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, length)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Purescreeps.Colony (Colony(..))
import Purescreeps.Harvest (findCorrespondingSource)
import Purescreeps.ReturnCode (Status, toStatus, orElse)
import Purescreeps.Work (findConstructionSites, findMyEmptyExtensions, findMyEmptySpawns, findMyEmptyTowers)
import Screeps.BodyPartType (BodyPartType, part_carry, part_move, part_work)
import Screeps.ConstructionSite (ConstructionSite)
import Screeps.Controller (Controller)
import Screeps.Creep (Creep, build, harvestSource, moveOpts, moveTo', transferToStructure, upgradeController)
import Screeps.Resource (resource_energy)
import Screeps.ReturnCode (ReturnCode)
import Screeps.Room (controller)
import Screeps.RoomObject (class RoomObject, AnyRoomObject, asAnyRoomObject)
import Screeps.Source (Source)
import Screeps.Spawn (Spawn)
import Screeps.Stores (storeTotalFree, storeTotalUsed)
import Screeps.Structure (class Structure)
import Screeps.Types (TargetPosition(..))

type CreepMemory
  = { spawn :: Spawn }

type BodySpec
  = Array BodyPartType

foreign import clearDeadCreepMemory :: Effect Unit

genericCreep :: Int → BodySpec
genericCreep capacity
  | capacity < 50 = []

genericCreep capacity
  | capacity < 100 = [ part_move ]

genericCreep capacity
  | capacity < 200 = [ part_carry, part_move ]

genericCreep capacity
  | capacity < 250 = [ part_work, part_carry, part_move ]

genericCreep capacity = concat [ genericCreep (capacity - 200), [ part_work, part_carry, part_move ] ]

type Job
  = Creep → Effect Status

toTargetToJob :: forall a. RoomObject a ⇒ (Creep → a → Effect ReturnCode) → (a → Job)
toTargetToJob f = (\target creep → (liftM1 toStatus) (f creep target))

moveToAction :: forall a. RoomObject a ⇒ Job → a → Job
moveToAction job =
  ( \target creep →
      job creep
        >>= orElse
            ( toTargetToJob (\c t → (moveTo' c (TargetObj t) (moveOpts { visualizePathStyle = Just {} }))) target creep
            )
  )


type Target
  = Tuple Job AnyRoomObject

harvestSource' :: Source → Job
harvestSource' = toTargetToJob harvestSource

upgradeController' :: Controller → Job
upgradeController' = toTargetToJob upgradeController

--transferEnergyToStructure' :: ∀ s. Structure s ⇒ s → Job
transferEnergyToStructure' :: ∀ s. Structure s ⇒ s → Job
transferEnergyToStructure' = toTargetToJob (\c t → transferToStructure c t resource_energy)

buildConstructionSite' :: ConstructionSite → Job
buildConstructionSite' = toTargetToJob build

runTarget :: Target → Job
runTarget (Tuple job target) =
  ( \creep → case { source: findCorrespondingSource creep } of
      { source: Just source } →
        ( if storeTotalUsed creep == 0 then
           (moveToAction (harvestSource' source) source) creep
          else
            if storeTotalFree creep == 0 then
              (moveToAction job target) creep
            else
              (harvestSource' source creep >>= orElse ((moveToAction job target) creep))
        )
      { source: _ } → pure $ Left "No source/controller found"
  )

generateControllerUpgradeTargets :: ∀ f g. Foldable f ⇒ Foldable g ⇒ f Colony → g Creep → Array Target
generateControllerUpgradeTargets colonies creeps =
  ( foldMap
      ( \(Colony room) →
          (replicate (length creeps) (controller room)) # catMaybes
            # map
                ( \controller →
                    Tuple (upgradeController' controller) (asAnyRoomObject controller)
                )
      )
      colonies
  )

generateFillStoreTargets ::  ∀ f g. Foldable f ⇒ Foldable g ⇒ f Colony → g Creep → Array Target
generateFillStoreTargets colonies creeps =
  ( foldMap
      ( \(Colony room) →
          ((findMyEmptySpawns room) <> (findMyEmptyExtensions room) <> (findMyEmptyTowers room))
            # map
                ( \store →
                  Tuple (transferEnergyToStructure' store) (asAnyRoomObject store)
                )
      )
      colonies
  )

generateBuildTargets ::  ∀ f g. Foldable f ⇒ Foldable g ⇒ f Colony → g Creep → Array Target
generateBuildTargets colonies creeps =
  ( foldMap
      ( \(Colony room) →
          (findConstructionSites room)
            # map
                ( \site →
                    Tuple (buildConstructionSite' site) (asAnyRoomObject site)
                )
      )
      colonies
  )

generateTargets ::  ∀ f g. Foldable f ⇒ Foldable g ⇒ f Colony → g Creep → Array Target
generateTargets colonies creeps = generateFillStoreTargets colonies creeps <> generateBuildTargets colonies creeps <> generateControllerUpgradeTargets colonies creeps

assignTargets :: Array Target → Array (Tuple String Creep) → Effect (Array (Tuple String Status))
assignTargets targets creeps = (traverse sequence) (zipWith (\target creep → runTarget target <$> creep) targets creeps)
