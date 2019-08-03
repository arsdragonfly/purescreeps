module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Screeps.Game as Game

main :: Effect Unit
main = do
  startUsed <- Game.getUsedCpu
  log $ "Start: " <> show startUsed
