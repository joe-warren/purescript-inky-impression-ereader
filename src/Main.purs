module Main
  ( main
  )
  where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Aff (Aff, launchAff, delay, launchAff_)
import Data.Time.Duration (Milliseconds (..))
import Buttons as Buttons
import Image as Image
import Options.Applicative as OA
import Control.Alt ((<|>))
import Data.Either (Either(..))
import RPiDisplay as RPiDisplay
import WindowedDisplay as WindowedDisplay
import Streams as SR
import Streams ((>->))
import Component as Component
import GalleryComponent as GalleryComponent

displayPipeline :: Buttons.Mode -> SR.Stream (Image.FullscreenImage) (Unit) Aff Unit
displayPipeline Buttons.RPiMode = SR.sApp (RPiDisplay.display) 
displayPipeline Buttons.WindowedMode = SR.sApp (WindowedDisplay.display) 

main :: Effect Unit
main = 
  let prefs = (OA.fullDesc <> OA.progDesc "EReader Software") 
   in join <<< OA.execParser <<< ((flip OA.info) prefs) $ ado
        mode <- OA.flag' Buttons.WindowedMode ( OA.long "windowed" ) <|> pure Buttons.RPiMode
        in do
              {--_ <- launchAff $ do
                  let im = Image.loadSizedPalettizedImage Image.screenWidthHalf Image.screenHeightHalf "assets/large-book-test-quarter.png"
                  let im2 = Image.concatH im im
                  let im3 = Image.concatV im2 im2
                  case mode of 
                    Buttons.RPiMode -> RPiDisplay.display im3
                    Buttons.WindowedMode -> WindowedDisplay.display im3--}
              log "📖"

              launchAff_ $ do 
                 component <- GalleryComponent.galleryComponent "photos"
                 let stream = Buttons.buttonPipeline mode >-> Component.componentToStream component >-> displayPipeline mode >-> SR.drain
                 (SR.runStream stream)