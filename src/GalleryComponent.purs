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
import ZipperArray as ZA

type GalleryState = ZA.ZipperArray String

imageFiles :: Array Pattern
imageFiles = Pattern <$> [".png", ".gif", ".jpg"]

isImage :: String -> Aff Boolean
isImage path = do 
    s <- FS.stat path
    pure $ if Stats.isDirectory s 
        then false
        else any (isJust <<< (_ `String.stripSuffix` (String.toLower path))) imageFiles

galleryComponent :: String -> String -> Aff (EReaderComponent (GalleryState))
galleryComponent dir file = do
    files <- (Array.filterA isImage <<< (map ((dir <> "/") <> _))) =<< FS.readdir dir
    let noFiles = liftEffect $ throw ("empty directory: " <> dir) 
    stateAtStart <- maybe noFiles pure (ZA.fromArray files)
    let state = fromMaybe stateAtStart $ ZA.focusWith (_ == file) stateAtStart
    let update (Tuple buttonId press) st = pure $ case press of 
            Buttons.ShortTap -> case buttonId of 
                Buttons.Button1 -> fromMaybe (ZA.goLast st) $ ZA.goPrev st
                Buttons.Button4 -> fromMaybe (ZA.goFirst st) $ ZA.goNext st
                _ -> st
            _ -> st
    let render st = Image.loadArbitraryFullscreenImage (ZA.current st)

    pure $ {
        initialState: state,
        updateComponent: update,
        renderComponent: render    
    }