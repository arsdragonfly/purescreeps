module Purescreeps.ReturnCode where

import Prelude
import Data.Either (Either(..), either)
import Effect (Effect)
import Screeps.ReturnCode (ReturnCode, ok)

toEither :: ReturnCode → Either ReturnCode ReturnCode
toEither c = if c == ok then Right c else Left c

branch :: ∀ a. a → a → ReturnCode → a
branch x y c = either (\_ → x) (\_ → y) (toEither c)

orElse :: Effect ReturnCode → ReturnCode → Effect ReturnCode
orElse eff code = branch (eff) (pure code) code
