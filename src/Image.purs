module Image(PalettizedImage (..), loadPalettizedImage) where

import Prelude

import Foreign (Foreign)
import Control.Promise as Promise

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Data.Either (Either(..))
import Control.Monad ((=<<))

data PalettizedImage = PalettizedImage Foreign

foreign import openPalettized :: (String -> Either String PalettizedImage) -> (Foreign -> Either String PalettizedImage) -> String -> Effect (Promise.Promise (Either String PalettizedImage))


loadPalettizedImage :: String -> Aff (Either String PalettizedImage)
loadPalettizedImage filename = Promise.toAffE (openPalettized (Left) (Right <<< PalettizedImage) filename)