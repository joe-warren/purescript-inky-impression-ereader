module WindowedDisplay (display) where


import Prelude

import Foreign (Foreign)
import Control.Promise as Promise
import Effect.Aff (Aff)
import Effect (Effect)
import Effect.Class.Console (log)
import Data.Either (Either (..))

import Image (Sized(..), ScreenHeight, ScreenWidth, PalettizedImage(..), runPalettizedImage)

foreign import windowedDisplayRaw :: Foreign -> Effect (Promise.Promise Foreign)


display :: Sized ScreenWidth ScreenHeight PalettizedImage -> Aff Unit
display (Sized img) = do
  d <- runPalettizedImage img
  case d of 
     Left err -> log err
     Right raw -> void <<< Promise.toAffE $  windowedDisplayRaw raw