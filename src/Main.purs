module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log, logShow)
import Purescreeps.Harvest (findCorrespondingSource)
import Purescreeps.ReturnCode (orElse)
import Purescreeps.Spawn (createCreepsForAllColonies)
import Screeps.Creep (Creep, harvestSource, moveOpts, moveTo', upgradeController)
import Screeps.Game (creeps)
import Screeps.ReturnCode (ReturnCode)
import Screeps.Room (controller)
import Screeps.RoomObject (room)
import Screeps.Stores (storeTotalFree, storeTotalUsed)
import Screeps.Types (TargetPosition(..))

main :: Effect Unit
main = do
  createCreepsForAllColonies >>= logShow
  void $ creeps >>= traverse runCreep

runCreep :: Creep → Effect Unit
runCreep creep = case { source: findCorrespondingSource creep, controller: controller (room creep) } of
  { source: Just source, controller: Just controller } → do
    returnCode ←
      ( if storeTotalUsed creep == 0 then
          (moveToAction harvestSource creep source)
        else
          if storeTotalFree creep == 0 then
            (moveToAction upgradeController creep controller)
          else
            (harvestSource creep source >>= orElse (moveToAction upgradeController creep controller))
      )
    logShow returnCode
  { source: _, controller: _ } → log "No source/controller found"

moveToAction :: forall a. (Creep → a → Effect ReturnCode) → Creep → a → Effect ReturnCode
moveToAction action creep target =
  action creep target
    >>= orElse (moveTo' creep (TargetObj target) (moveOpts { visualizePathStyle = Just {} }))
