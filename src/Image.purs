module Image
  ( PalettizedImage(..)
  , Sized (..)
  , loadPalettizedImage
  , loadSizedPalettizedImage
  , screenWidth
  , screenHeight
  ) where

import Prelude

import Foreign (Foreign)
import Control.Promise as Promise

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Data.Either (Either(..))
import Control.Monad ((=<<))
import  Data.Typelevel.Num.Sets 
import  Data.Typelevel.Num.Reps
import Type.Proxy (Proxy (..))
import Data.Int (round)
import Data.Array as Array
import Data.Tuple (Tuple (..))
import Data.Maybe (Maybe (..))

newtype PalettizedImage = PalettizedImage Foreign

newtype Sized w h a = Sized a

type ScreenWidth = (D6 :* D0) :* D0
type ScreenHeight = (D4 :* D4) :* D8

screenWidth :: Proxy ScreenWidth
screenWidth = Proxy 

screenHeight :: Proxy ScreenHeight
screenHeight = Proxy

foreign import openPalettized :: (String -> Either String PalettizedImage) -> (Foreign -> Either String PalettizedImage) -> String -> Effect (Promise.Promise (Either String PalettizedImage))

foreign import size :: (Foreign) -> Effect (Promise.Promise (Array Number))

loadPalettizedImage :: String -> Aff (Either String PalettizedImage)
loadPalettizedImage filename = Promise.toAffE (openPalettized (Left) (Right <<< PalettizedImage) filename)

checkSize ::forall w h. Pos w => Pos h => Proxy w -> Proxy h -> PalettizedImage -> Aff (Either String (Sized w h PalettizedImage))
checkSize w h img@(PalettizedImage raw) = do
    sz <- Promise.toAffE (size raw)
    let wExpected = toInt' w 
    let hExpected = toInt' h
    let wActual = round <$> Array.index sz 0 
    let hActual = round <$> Array.index sz 1
    if (Tuple wActual hActual == Tuple (Just wExpected) (Just hExpected)) 
        then pure <<< Left $ "unexpected size (got: " <> show wActual <> ", " <> show hActual <> " expected " <> show wExpected <> ", " <> show hExpected <> ")"
        else pure <<< Right <<< Sized $ img

loadSizedPalettizedImage :: forall w h. Pos w => Pos h => Proxy w -> Proxy h -> String -> Aff (Either String (Sized w h PalettizedImage))
loadSizedPalettizedImage w h path = do
    im <- loadPalettizedImage path
    case im of
        Left e -> pure $ Left e
        Right img -> checkSize w h img

