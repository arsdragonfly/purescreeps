module Purescreeps.Context where

import Prelude

import Data.Array (snoc)
import Data.Either (either)
import Data.Foldable (class Foldable, foldMap, foldl)
import Data.List (List, concat, fromFoldable)
import Data.Map (singleton)
import Data.Map.Monoidal (MonoidalMap(..))
import Data.Monoid.Additive (Additive(..))
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

updateContext :: Context → Effect Context
updateContext context = do
  colonies ← findColonies
  pure $ { footprints: snoc context.footprints (getFootprintForCurrentTick colonies) }

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

displayFootprintForCurrentTick :: ∀ t. Traversable t ⇒ t Colony → Effect (t RoomVisual)
displayFootprintForCurrentTick colonies =
  traverse
    ( \(Colony room) → (getRoomVisual room) >>= (getFootprint (Colony room) # displayFootprint)
    )
    colonies

flattenFootprints :: List (Array RoomPosition) → List RoomPosition
flattenFootprints = map fromFoldable >>> concat

type Counter k = MonoidalMap k (Additive Int)

countFootprints :: List RoomPosition → Counter RoomPosition
countFootprints = foldMap (\rp → MonoidalMap (singleton rp (Additive 1)))
