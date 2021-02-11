module Purescreeps.Context where

import Prelude

import Data.Array (slice, length, snoc)
import Data.Either (either)
import Data.Foldable (class Foldable, foldMap, foldl)
import Data.Map (Map, singleton, keys)
import Data.Monoid.Additive (Additive(..))
import Data.Set (Set)
import Data.Traversable (class Traversable, traverse)
import Effect (Effect)
import Foreign.Object (Object)
import Purescreeps.Colony (Colony(..), findColonies)
import Screeps.FindType (find_my_creeps)
import Screeps.Memory (get, getMemoryGlobal, set)
import Screeps.Room (find)
import Screeps.RoomObject (pos)
import Screeps.RoomPosition.Type (RoomPosition)
import Screeps.RoomVisual (RoomVisual, circle, getRoomVisual)

type Context
  = { footprints :: Array (Array RoomPosition) }

type Context2 = { footprints :: Object (Array Number) }

defaultContext :: Context
defaultContext = { footprints: mempty }

getContext :: Effect Context
getContext = do
  memory ← getMemoryGlobal
  result ← get memory "context"
  pure $ either (const defaultContext) identity result

setContext :: Context → Effect Unit
setContext context = do
  memory ← getMemoryGlobal
  set memory "context" context

getFootprint :: Colony → Array RoomPosition
getFootprint (Colony room) = find room find_my_creeps # map pos

getFootprintForCurrentTick :: ∀ f. Foldable f ⇒ f Colony → Array RoomPosition
getFootprintForCurrentTick = foldMap getFootprint

displayFootprint :: ∀ f. Foldable f ⇒ f RoomPosition → RoomVisual → Effect RoomVisual
displayFootprint rpss rv = foldl (\erv rps → erv >>= circle rps) (pure rv) rpss

flattenFootprints :: Array (Array RoomPosition) → Array RoomPosition
flattenFootprints = join

type Counter k = Map k (Additive Int)

countFootprints :: ∀ f. Foldable f ⇒ f RoomPosition → Counter RoomPosition
countFootprints = foldMap (\rp → singleton rp (Additive 1))

countFootprintsFromContext :: Effect (Set RoomPosition)
countFootprintsFromContext = do
  context ← getContext
  pure (flattenFootprints context.footprints # countFootprints # keys)

displayFootprintFromContext :: ∀ t. Traversable t ⇒ t Colony → Effect (t RoomVisual)
displayFootprintFromContext colonies =
  traverse
    ( \(Colony room) → do
      rvs ← getRoomVisual room
      rpss ← countFootprintsFromContext
      displayFootprint rpss rvs
    )
    colonies

updateContext :: Context → Effect Context
updateContext context = do
  colonies ← findColonies
  pure $ { footprints: slice (max 0 (length context.footprints - 50)) (length context.footprints + 1) (snoc context.footprints (getFootprintForCurrentTick colonies)) }
