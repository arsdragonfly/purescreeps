module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log, logShow)
import Purescreeps.Harvest (findCorrespondingSource)
import Purescreeps.ReturnCode (orElse)
import Purescreeps.Spawn (createCreepsForAllColonies)
import Screeps.Types (TargetPosition(..))
import Screeps.Creep (Creep, harvestSource, moveOpts, moveTo')
import Screeps.Game (creeps)
import Screeps.RoomObject (room)

main :: Effect Unit
main = do
  createCreepsForAllColonies >>= logShow
  void $ creeps >>= traverse runCreep

runCreep :: Creep → Effect Unit
runCreep creep = case findCorrespondingSource creep of
    Just source →
      ( harvestSource creep source
          >>= orElse (moveTo' creep (TargetObj source) (moveOpts { visualizePathStyle = Just {} }))
          >>= logShow
      )
    Nothing → log "No source found"
