module GalleryComponent where

import Prelude

import Buttons as Buttons
import Image as Image
import Component
import Node.FS.Aff as FS
import ZipperArray as ZA
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Data.Maybe (Maybe (..), fromMaybe, maybe)
import Effect.Exception (throw)
import Data.Tuple (Tuple (..))

galleryComponent :: String -> Aff (EReaderComponent (ZA.ZipperArray String))
galleryComponent dir = do
    files <- FS.readdir dir
    let files' = ((dir <> "/") <> _) <$> files 
    let noFiles = liftEffect $ throw ("empty directory: " <> dir) 
    state <- maybe noFiles pure (ZA.fromArray files')
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