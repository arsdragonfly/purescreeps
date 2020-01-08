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
import Screeps.Progress (progressTotal)
import Screeps.Refillable (energy, energyCapacity)
import Screeps.Room (controller, find')
import Screeps.RoomObject (pos)
import Screeps.RoomPosition (FindContext(..), findClosestByRange, findClosestByRange')
import Screeps.Structure (AnyStructure, fromAnyStructure, structureType, structure_rampart, structure_tower, structure_wall)
import Screeps.Tower (Tower, attack, repair)

runTower :: Int → Tower → Effect Status
runTower hp tower =
  ( chain closestHostile attack
      ( chain' closestDamagedStructure repair
          ( chain' closestDamagedBarrier repair
              ( pure $ Left "Nothing to do."
              )
          )
      )
  )
  where
  closestHostile = findClosestByRange (pos tower) (OfType find_hostile_creeps)

  closestDamagedStructure =
    findClosestByRange' (pos tower) (OfType find_structures)
      ( \s →
          (hits s) < (hitsMax s)
            && structureType s
            /= structure_rampart
            && structureType s
            /= structure_wall
      )

  closestDamagedBarrier =
    findClosestByRange' (pos tower) (OfType find_structures)
      ( \s →
          (hits s) < (hitsMax s) && (hits s) < hp && energy tower * 2 > energyCapacity tower
      )

  chain target action next =
    either
      (const next)
      (maybe next ((action tower) >>> (liftM1 toStatus)))
      target

  chain' target action next =
    either
      (const next)
      (maybe next ((action tower) >>> (liftM1 toStatus)))
      target

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
runTowers colony = sequence $ map (\t → runTower (maxHits colony) t) $ findMyTowers colony

-- TODO: figure out if it works for level 8
maxHits :: Colony → Int
maxHits (Colony room) = maybe 0 (\c → progressTotal c) (controller room)
