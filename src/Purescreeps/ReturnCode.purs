module Purescreeps.ReturnCode where

import Prelude
import Data.Either (Either(..), either)
import Effect (Effect)
import Screeps.ReturnCode (ReturnCode, ok)

type Status = Either String Unit

toStatus :: ReturnCode → Status
toStatus c = if c == ok then Right unit else Left (show c)

orElse :: Effect Status → Status → Effect Status
orElse eff status = either (\l → eff) (\r → pure status) status
