module Main where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Effect.Console (logShow)
import Purescreeps.Colony (findColonies)
import Purescreeps.Creep (moveToAction, Job, harvestSource', upgradeController')
import Purescreeps.Harvest (findCorrespondingSource)
import Purescreeps.ReturnCode (orElse)
import Purescreeps.Spawn (createCreeps)
import Screeps.Game (creeps) as Game
import Screeps.Room (controller)
import Screeps.RoomObject (room)
import Screeps.Stores (storeTotalFree, storeTotalUsed)

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


