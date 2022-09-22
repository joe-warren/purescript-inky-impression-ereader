module RawRPiButtons where

import Prelude
import Prelude

import Effect (Effect)
import Control.Promise (Promise, toAffE)

foreign import runButtonsRaw :: (Int -> Boolean -> Effect Unit) -> Effect (Promise Unit)
