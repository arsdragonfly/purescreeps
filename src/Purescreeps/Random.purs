module Purescreeps.Random where

import Prelude
import Data.Array (index, length)
import Data.Char (toCharCode)
import Data.Foldable (foldl)
import Data.Int.Bits (shl)
import Data.Maybe (Maybe)
import Data.String.CodeUnits (toCharArray)
import Effect (Effect)
import Effect.Console as Console
import Effect.Random (randomInt)
import Run (FProxy, Run, SProxy(..), Step(..), EFFECT, interpret, liftEffect, match, on, runAccumPure, runBaseEffect, runCont, send)
import Run as Run

sample :: ∀ a. Array a → Effect (Maybe a)
sample t = (index t) <$> (randomInt 0 $ (length t) - 1)

hashCode :: String → Int
hashCode s = foldl (\hash → \c → (shl hash 5) - hash + toCharCode c) 0 (toCharArray s)

sampleByStringHash :: ∀ a s. Show s => s → Array a → Maybe a
sampleByStringHash str arr = index arr $ (mod (hashCode $ show str) (length arr))

-- purescript-run experiment
data TalkF a
  = Speak String a
  | Listen (String → a)

derive instance functorTalkF :: Functor TalkF

type TALK
  = FProxy TalkF

_talk = SProxy :: SProxy "talk"

speak :: forall r. String → Run ( talk :: TALK | r ) Unit
speak str = Run.lift _talk (Speak str unit)

listen :: forall r. Run ( talk :: TALK | r ) String
listen = Run.lift _talk (Listen identity)

handleTalk :: forall r. TalkF ~> Run ( effect :: EFFECT | r )
handleTalk = case _ of
  Speak str next → do
    liftEffect $ Console.log str
    pure next
  Listen reply → do
    pure (reply "I am Groot")

runTalk ::
  forall r.
  Run ( effect :: EFFECT, talk :: TALK | r )
    ~> Run ( effect :: EFFECT | r )
runTalk = interpret (on _talk handleTalk send)

type IsThereMore
  = Boolean

type Bill
  = Int

data Food
  = Pizza
  | Chizburger

data DinnerF a
  = Eat Food (IsThereMore -> a)
  | CheckPlease (Bill -> a)

derive instance functorDinnerF :: Functor DinnerF

type DINNER
  = FProxy DinnerF

_dinner = SProxy :: SProxy "dinner"

eat :: forall r. Food -> Run ( dinner :: DINNER | r ) IsThereMore
eat food = Run.lift _dinner (Eat food identity)
