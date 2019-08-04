module Main where

import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, bind, ($), (<>), show)
import Purescreeps.Spawn (createCreepsForAllColonies)

main :: Effect Unit
main = do
  results <- createCreepsForAllColonies
  log $ "Start: " <> show results
