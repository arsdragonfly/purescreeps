module Purescreeps.Colony where

import Prelude

import Data.Array (null)
import Data.List (List, fromFoldable)
import Data.Map (filter)
import Effect (Effect)
import Screeps.FindType (find_my_spawns)
import Screeps.Game as Game
import Screeps.Room (find)
import Screeps.RoomObject (Room)

data Colony = Colony Room

instance showColony :: Show Colony where
  show (Colony room) = show room

findColonies :: Effect (List Colony)
findColonies = do
  rooms <- Game.rooms
  roomsWithSpawns <- pure $ filter (\r -> not <<< null $ find r find_my_spawns) rooms
  pure $ (fromFoldable <<< map Colony) roomsWithSpawns

-- findColonies :: Effect ( Array Colony )
-- findColonies = do
--     rooms <- fold Game.rooms
