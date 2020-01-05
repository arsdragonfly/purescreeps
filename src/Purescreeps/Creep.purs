module Purescreeps.Creep where

import Prelude

import Data.Array (catMaybes, concat, foldMap, replicate, zipWith)
import Data.Either (Either(..))
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (class Foldable, length)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple)
import Effect (Effect)
import Purescreeps.Colony (Colony(..))
import Purescreeps.Harvest (findCorrespondingSource)
import Purescreeps.ReturnCode (Status, toStatus, orElse)
import Purescreeps.Work (findMyEmptySpawns)
import Screeps.BodyPartType (BodyPartType, part_carry, part_move, part_work)
import Screeps.Controller (Controller)
import Screeps.Creep (Creep, harvestSource, moveOpts, moveTo', transferToStructure, upgradeController)
import Screeps.Resource (resource_energy)
import Screeps.ReturnCode (ReturnCode)
import Screeps.Room (controller)
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
  | capacity < 150 = [ part_carry, part_move ]

genericCreep capacity
  | capacity < 250 = [ part_work, part_carry, part_move ]

genericCreep capacity = concat [ genericCreep (capacity - 200), [ part_work, part_carry, part_move ] ]

type Job
  = Creep → Effect Status

toTargetToJob :: forall a. (Creep → a → Effect ReturnCode) → (a → Job)
toTargetToJob f = (\target creep → (liftM1 toStatus) (f creep target))

moveToAction :: forall a. (a → Job) → (a → Job)
moveToAction action =
  ( \target creep →
      action target creep
        >>= orElse
            ( toTargetToJob (\c t → (moveTo' c (TargetObj t) (moveOpts { visualizePathStyle = Just {} }))) target creep
            )
  )

data TargetF a
  = TargetF (a → Job) a

type Target
  = Exists TargetF

harvestSource' :: Source → Job
harvestSource' = toTargetToJob harvestSource

upgradeController' :: Controller → Job
upgradeController' = toTargetToJob upgradeController

transferEnergyToStructure' :: ∀ s. Structure s ⇒ s → Job
transferEnergyToStructure' = toTargetToJob (\c t → transferToStructure c t resource_energy)

runTarget :: Target → Job
runTarget = runExists runTarget'
  where
  runTarget' :: forall a. TargetF a -> Job
  runTarget' (TargetF action target) =
    ( \creep → case { source: findCorrespondingSource creep } of
        { source: Just source } →
          ( if storeTotalUsed creep == 0 then
              (moveToAction harvestSource' source creep)
            else
              if storeTotalFree creep == 0 then
                (moveToAction action target creep)
              else
                (harvestSource' source creep >>= orElse (moveToAction action target creep))
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
                    mkExists $ TargetF upgradeController' controller
                )
      )
      colonies
  )

generateFillStoreTargets ::  ∀ f g. Foldable f ⇒ Foldable g ⇒ f Colony → g Creep → Array Target
generateFillStoreTargets colonies creeps =
  ( foldMap
      ( \(Colony room) →
          findMyEmptySpawns room
            # map
                ( \store →
                    mkExists $ TargetF transferEnergyToStructure' store
                )
      )
      colonies
  )

generateTargets ::  ∀ f g. Foldable f ⇒ Foldable g ⇒ f Colony → g Creep → Array Target
generateTargets colonies creeps = generateFillStoreTargets colonies creeps <> generateControllerUpgradeTargets colonies creeps

assignTargets :: Array Target → Array (Tuple String Creep) → Effect (Array (Tuple String Status))
assignTargets targets creeps = (traverse sequence) (zipWith (\target creep → runTarget target <$> creep) targets creeps)
