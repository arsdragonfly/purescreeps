module Main where

import Prelude
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log, logShow)
import Purescreeps.ReturnCode (orElse)
import Purescreeps.Spawn (createCreepsForAllColonies)
import Screeps (TargetPosition(..), Creep)
import Screeps.Creep (harvestSource, moveTo)
import Screeps.FindType (find_sources)
import Screeps.Game (creeps)
import Screeps.Room (find)
import Screeps.RoomObject (Room, room)
import Screeps.Source (Source)

main :: Effect Unit
main = do
  createCreepsForAllColonies >>= logShow
  void $ creeps >>= traverse runCreep

findFirstSource :: Room → Maybe Source
findFirstSource r = head $ find r find_sources

runCreep :: Creep → Effect Unit
runCreep creep = case findFirstSource $ room creep of
  Just source →
    ( harvestSource creep source
        >>= orElse (moveTo creep $ TargetObj source)
        >>= logShow
    )
  Nothing → log "No source found"
