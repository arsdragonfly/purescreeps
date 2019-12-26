module Main where

import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, bind, ($), (<>), show, discard, void)
import Purescreeps.Spawn (createCreepsForAllColonies)
import Data.Traversable (traverse)
import Screeps.Creep (say)
import Screeps.Game (creeps)

main :: Effect Unit
main = do
  results <- createCreepsForAllColonies
  log $ "Start: " <> show results
  map <- creeps
  void $ traverse (\c -> say c "blyat!") map
