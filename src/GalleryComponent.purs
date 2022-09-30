module GalleryComponent where

import Component
import Prelude

import Buttons as Buttons
import Data.Array as Array
import Data.Foldable (any)
import Data.Maybe (Maybe(..), fromMaybe, maybe, isJust)
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Image as Image
import Node.FS.Aff as FS
import Node.FS.Stats as Stats
import Type.Proxy (Proxy(..))
import ZipperArray as ZA

data Rotation = NoRotation | ClockwiseRotation

data GalleryState = GalleryState Rotation (ZA.ZipperArray String)

hasExtension :: Array String -> String -> Aff Boolean
hasExtension exts path = do
    s <- FS.stat path
    let patterns = Pattern <<< ("." <> _) <<< String.toLower <$> exts
    pure $ if Stats.isDirectory s 
        then false
        else any (isJust <<< (_ `String.stripSuffix` (String.toLower path))) patterns

isImage :: String -> Aff Boolean
isImage  = hasExtension ["png", "gif", "jpg"]

isPdf :: String -> Aff Boolean
isPdf = hasExtension ["pdf"]

changeRotationState :: Rotation -> Rotation
changeRotationState NoRotation = ClockwiseRotation
changeRotationState ClockwiseRotation = NoRotation

galleryComponent :: String -> String -> Aff (EReaderComponent (GalleryState))
galleryComponent dir file = do
    files <- (Array.filterA isImage <<< (map ((dir <> "/") <> _))) =<< FS.readdir dir
    let noFiles = liftEffect $ throw ("empty directory: " <> dir) 
    stateAtStart <- maybe noFiles pure (ZA.fromArray files)
    let state = fromMaybe stateAtStart $ ZA.focusWith (_ == file) stateAtStart
    let update (Tuple buttonId press) g@(GalleryState r st) = pure $ case press of 
            Buttons.ShortTap -> case buttonId of 
                Buttons.Button1 -> GalleryState r $ fromMaybe (ZA.goLast st) $ ZA.goPrev st
                Buttons.Button2 -> GalleryState (changeRotationState r) st
                Buttons.Button4 -> GalleryState r $ fromMaybe (ZA.goFirst st) $ ZA.goNext st
                _ -> g
            _ -> g
    let render (GalleryState r st) = case r of
            NoRotation -> Image.loadArbitraryFullscreenImage (ZA.current st)
            ClockwiseRotation -> Image.rotate $ Image.loadSizedArbitraryImage Proxy Proxy (ZA.current st)
    pure $ {
        initialState: GalleryState NoRotation state,
        updateComponent: update,
        renderComponent: render    
    }