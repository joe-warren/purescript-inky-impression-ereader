module GalleryComponent where

import Component
import Prelude

import Buttons as Buttons
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Image as Image
import Node.FS.Aff as FS
import ZipperArray as ZA

type GalleryState = ZA.ZipperArray String

galleryComponent :: String -> String -> Aff (EReaderComponent (GalleryState))
galleryComponent dir file = do
    files <- FS.readdir dir
    let files' = ((dir <> "/") <> _) <$> files 
    let noFiles = liftEffect $ throw ("empty directory: " <> dir) 
    stateAtStart <- maybe noFiles pure (ZA.fromArray files')
    let state = fromMaybe stateAtStart $ ZA.focusWith (_ == file) stateAtStart
    let update (Tuple buttonId press) st = pure $ case press of 
            Buttons.ShortTap -> case buttonId of 
                Buttons.Button1 -> fromMaybe st $ ZA.goPrev st
                Buttons.Button4 -> fromMaybe st $ ZA.goNext st
                _ -> st
            _ -> st
    let render st = Image.loadArbitraryFullscreenImage (ZA.current st)

    pure $ {
        initialState: state,
        updateComponent: update,
        renderComponent: render    
    }