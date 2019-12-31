module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (logShow)
import Purescreeps.Harvest (findCorrespondingSource)
import Purescreeps.ReturnCode (orElse)
import Purescreeps.Spawn (createCreepsForAllColonies)
import Purescreeps.Creep (moveToAction)
import Screeps.Creep (Creep, harvestSource, upgradeController)
import Screeps.Game (creeps)
import Screeps.Room (controller)
import Screeps.RoomObject (room)
import Screeps.Stores (storeTotalFree, storeTotalUsed)

main :: Effect Unit
main = do
  createCreepsForAllColonies >>= logShow
  creeps >>= traverse runCreep >>= logShow

runCreep :: Creep → Effect String
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
    pure $ show returnCode
  { source: _, controller: _ } → pure $ "No source/controller found"
