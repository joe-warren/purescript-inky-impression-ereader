module Image
  ( PalettizedImage(..)
  , ScreenHeight
  , ScreenHeightHalf
  , ScreenWidth
  , Sized(..)
  , concatH
  , concatHRaw
  , concatV
  , loadPalettizedImage
  , loadSizedPalettizedImage
  , runPalettizedImage
  , screenHeight
  , screenHeightHalf
  , screenWidth
  , screenWidthHalf
  )
  where

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
import  Data.Typelevel.Num.Ops (class Add)
import Type.Proxy (Proxy (..))
import Data.Int (round)
import Data.Array as Array
import Data.Tuple (Tuple (..))
import Data.Maybe (Maybe (..))
import Control.Monad.Except.Trans (ExceptT(..), runExceptT, throwError, lift)
newtype PalettizedImage = PalettizedImage (ExceptT String Aff Foreign)


runPalettizedToExcept :: PalettizedImage -> ExceptT String Aff Foreign
runPalettizedToExcept (PalettizedImage p) = p

runPalettizedImage :: PalettizedImage -> Aff (Either String Foreign)
runPalettizedImage = runExceptT <<< runPalettizedToExcept

newtype Sized w h a = Sized a

type ScreenWidth = (D6 :* D0) :* D0
type ScreenHeight = (D4 :* D4) :* D8

screenWidth :: Proxy ScreenWidth
screenWidth = Proxy 

screenHeight :: Proxy ScreenHeight
screenHeight = Proxy

type ScreenWidthHalf = (D3 :* D0) :* D0

screenWidthHalf :: Proxy ScreenWidthHalf
screenWidthHalf = Proxy

type ScreenHeightHalf = (D2 :* D2) :* D4

screenHeightHalf :: Proxy ScreenHeightHalf
screenHeightHalf = Proxy

foreign import openPalettized :: (String -> Either String Foreign) -> (Foreign -> Either String Foreign) -> String -> Effect (Promise.Promise (Either String Foreign))

foreign import size :: (Foreign) -> Effect (Promise.Promise (Array Number))

foreign import concatHRaw :: Foreign -> Foreign -> Effect (Promise.Promise (Foreign))
foreign import concatVRaw :: Foreign -> Foreign -> Effect (Promise.Promise (Foreign))

loadPalettizedImage :: String -> PalettizedImage
loadPalettizedImage filename = PalettizedImage <<< ExceptT $ Promise.toAffE (openPalettized (Left) (Right) filename)

checkSize ::forall w h. Pos w => Pos h => Proxy w -> Proxy h -> PalettizedImage -> (Sized w h PalettizedImage)
checkSize w h img = Sized <<< PalettizedImage $ do
    raw <- runPalettizedToExcept img
    sz <- lift $ Promise.toAffE (size raw)
    let wExpected = toInt' w 
    let hExpected = toInt' h
    let wActual = round <$> Array.index sz 0 
    let hActual = round <$> Array.index sz 1
    if (Tuple wActual hActual /= Tuple (Just wExpected) (Just hExpected)) 
        then throwError $ "unexpected size (got: " <> show wActual <> ", " <> show hActual <> " expected " <> show wExpected <> ", " <> show hExpected <> ")"
        else pure raw

loadSizedPalettizedImage :: forall w h. Pos w => Pos h => Proxy w -> Proxy h -> String -> Sized w h PalettizedImage
loadSizedPalettizedImage w h path = checkSize w h (loadPalettizedImage path)


concatH :: forall w1 w2 wTot h. Pos w1 => Pos w2 => Pos h => Add w1 w2 wTot => Sized w1 h PalettizedImage -> Sized w2 h PalettizedImage -> Sized wTot h PalettizedImage
concatH (Sized a) (Sized b) = Sized <<< PalettizedImage $ do
    rawA <- runPalettizedToExcept a 
    rawB <- runPalettizedToExcept b
    lift $ Promise.toAffE (concatHRaw rawA rawB) 

concatV :: forall w h1 h2 hTot. Pos w => Pos h1 => Pos h2 => Add h1 h2 hTot => Sized w h1 PalettizedImage -> Sized w h2 PalettizedImage -> Sized w hTot PalettizedImage
concatV (Sized a) (Sized b) = Sized <<< PalettizedImage $ do 
    rawA <- runPalettizedToExcept a 
    rawB <- runPalettizedToExcept b
    lift $ Promise.toAffE (concatVRaw rawA rawB) 
