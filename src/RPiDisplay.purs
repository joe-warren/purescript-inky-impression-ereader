module RPiDisplay
  ( display
  )
  where

import Prelude

import Foreign (Foreign)
import Control.Promise as Promise
import Effect.Aff (Aff)
import Effect (Effect)

import Image (Sized(..), ScreenHeight, ScreenWidth, PalettizedImage(..))

foreign import rpiDisplayRaw :: Foreign -> Effect (Promise.Promise Foreign)

display :: Sized ScreenWidth ScreenHeight PalettizedImage -> Aff Unit
display (Sized (PalettizedImage img)) = void <<< Promise.toAffE $  rpiDisplayRaw img