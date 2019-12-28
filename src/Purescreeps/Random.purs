module Purescreeps.Random where

import Prelude (($))
import Data.Array (index, length)
import Data.Maybe (Maybe)
import Data.Functor ((<$>))
import Effect (Effect)
import Effect.Random (randomInt)
import Screeps.Source (Source)
import Screeps.FindType (find_sources)
import Screeps.Room (find)
import Screeps.RoomObject (Room)

sample :: ∀ a. Array a → Effect (Maybe a)
sample t = (index t) <$> (randomInt 0 $ length t)

findRandomSource :: Room → Effect (Maybe Source)
findRandomSource r = sample $ find r find_sources
