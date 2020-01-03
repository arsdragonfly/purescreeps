module Purescreeps.Work where

import Prelude
import Data.Array (catMaybes, filter)
import Data.Foldable (elem)
import Data.Maybe (Maybe)
import Screeps.FindType (find_structures)
import Screeps.Refillable (AnyRefillable, isNotFull, toRefillable)
import Screeps.Room (find')
import Screeps.RoomObject (Room)
import Screeps.Spawn (Spawn)
import Screeps.Structure (AnyStructure, fromAnyStructure, structureType, structure_extension, structure_spawn, structure_tower)

findMyEmptySpawns :: Room → Array AnyRefillable
findMyEmptySpawns room =
  find' room find_structures
    ( \s →
        elem (structureType s)
          [ structure_extension, structure_spawn, structure_tower ]
    )
    # (map (fromAnyStructure :: AnyStructure → Maybe Spawn))
    # catMaybes
    # (map toRefillable)
    # catMaybes
    # filter (\s → isNotFull s)
