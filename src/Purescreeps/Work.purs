module Purescreeps.Work where

import Prelude

import Data.Array (catMaybes, filter)
import Data.Foldable (elem)
import Data.Maybe (Maybe)
import Screeps.ConstructionSite (ConstructionSite)
import Screeps.Extension (Extension)
import Screeps.FindType (find_construction_sites, find_structures)
import Screeps.Refillable (AnyRefillable, isNotFull, toRefillable)
import Screeps.Room (find, find')
import Screeps.RoomObject (Room)
import Screeps.Spawn (Spawn)
import Screeps.Structure (AnyStructure, fromAnyStructure, structureType, structure_extension, structure_spawn, structure_tower)
import Screeps.Tower (Tower)

findMyEmptySpawns :: Room → Array AnyRefillable
findMyEmptySpawns room =
  find' room find_structures
    ( \s →
        elem (structureType s)
          [ structure_spawn ]
    )
    # (map (fromAnyStructure :: AnyStructure → Maybe Spawn))
    # catMaybes
    # (map toRefillable)
    # catMaybes
    # filter (\s → isNotFull s)

findMyEmptyExtensions :: Room → Array AnyRefillable
findMyEmptyExtensions room =
  find' room find_structures
    ( \s →
        elem (structureType s)
          [ structure_extension ]
    )
    # (map (fromAnyStructure :: AnyStructure → Maybe Extension))
    # catMaybes
    # (map toRefillable)
    # catMaybes
    # filter (\s → isNotFull s)

findMyEmptyTowers :: Room → Array AnyRefillable
findMyEmptyTowers room =
  find' room find_structures
    ( \s →
        elem (structureType s)
          [ structure_tower ]
    )
    # (map (fromAnyStructure :: AnyStructure → Maybe Tower))
    # catMaybes
    # (map toRefillable)
    # catMaybes
    # filter (\s → isNotFull s)

findConstructionSites :: Room → Array ConstructionSite
findConstructionSites room = find room find_construction_sites