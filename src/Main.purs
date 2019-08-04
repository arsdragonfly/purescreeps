module Main where

import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, bind, show, ($), (<>))
import Spawn (createCreeps)

main :: Effect Unit
main = do
  result <- createCreeps
  log $ "Start: " <> show result
