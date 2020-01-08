module Purescreeps.Tower where

import Prelude
import Data.Array (catMaybes, elem)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe, maybe)
import Data.Traversable (sequence)
import Effect (Effect)
import Purescreeps.Colony (Colony(..))
import Purescreeps.ReturnCode (Status, toStatus)
import Screeps.Destructible (hits, hitsMax)
import Screeps.FindType (find_hostile_creeps, find_structures)
import Screeps.Room (find')
import Screeps.RoomObject (pos)
import Screeps.RoomPosition (FindContext(..), findClosestByRange, findClosestByRange')
import Screeps.Structure (AnyStructure, fromAnyStructure, structureType, structure_tower)
import Screeps.Tower (Tower, attack, repair)

runTower :: Tower → Effect Status
runTower tower =
  either
    (const repairClosestDamagedStructure)
    (maybe repairClosestDamagedStructure ((attack tower) >>> (liftM1 toStatus)))
    closestHostile
  where
  repairClosestDamagedStructure =
    either
      (show >>> Left >>> pure)
      (maybe (pure $ Left "Nothing to do.") ((repair tower) >>> (liftM1 toStatus)))
      closestDamagedStructure
    where
    closestDamagedStructure = findClosestByRange' (pos tower) (OfType find_structures) (\s → (hits s) < (hitsMax s))

  closestHostile = findClosestByRange (pos tower) (OfType find_hostile_creeps)

findMyTowers :: Colony → Array Tower
findMyTowers (Colony room) =
  find' room find_structures
    ( \s →
        elem (structureType s)
          [ structure_tower ]
    )
    # (map (fromAnyStructure :: AnyStructure → Maybe Tower))
    # catMaybes

runTowers :: Colony → Effect (Array Status)
runTowers colony = sequence $ map (\t → runTower t) $ findMyTowers colony
