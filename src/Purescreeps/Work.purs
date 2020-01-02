module Purescreeps.Work where

import Prelude

import Data.Array (catMaybes, filter)
import Data.Foldable (elem)
import Screeps.FindType (find_structures)
import Screeps.Room (find')
import Screeps.RoomObject (Room)
import Screeps.Stores (AnyStore, storeTotalFree, toAnyStore)
import Screeps.Structure (structureType, structure_extension, structure_spawn, structure_tower)

findMyEmptyStores :: Room → Array AnyStore
findMyEmptyStores room =
  find' room find_structures
    ( \s →
        elem (structureType s)
          [ structure_extension, structure_spawn, structure_tower ]
    )
    # (map toAnyStore)
    # catMaybes
    # filter (\s → storeTotalFree s > 0)
