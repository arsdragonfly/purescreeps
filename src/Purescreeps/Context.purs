module Purescreeps.Context where

import Prelude
import Data.Foldable (class Foldable, foldMap, foldl)
import Data.List (List(..))
import Data.Traversable (class Traversable, traverse)
import Effect (Effect)
import Purescreeps.Colony (Colony(..))
import Screeps.FindType (find_my_creeps)
import Screeps.Room (find)
import Screeps.RoomObject (pos)
import Screeps.RoomPosition.Type (RoomPosition)
import Screeps.RoomVisual (RoomVisual, circle, getRoomVisual)

type Context
  = { footprints :: List (Array RoomPosition) }

defaultContext :: Context
defaultContext = { footprints: Nil }

getFootprint :: Colony → Array RoomPosition
getFootprint (Colony room) = find room find_my_creeps # map pos

getFootprintForCurrentTick :: ∀ f. Foldable f ⇒ f Colony → Array RoomPosition
getFootprintForCurrentTick = foldMap getFootprint

displayFootprint :: Array RoomPosition → RoomVisual → Effect RoomVisual
displayFootprint rpss rv = foldl (\erv rps → erv >>= circle rps) (pure rv) rpss

displayFootprintForCurrentTick :: ∀ t. Traversable t ⇒ t Colony → Effect (t RoomVisual)
displayFootprintForCurrentTick colonies =
  traverse
    ( \(Colony room) → (getRoomVisual room) >>= (getFootprint (Colony room) # displayFootprint)
    )
    colonies
