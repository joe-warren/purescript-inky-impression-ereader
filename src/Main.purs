module Main
  ( main
  )
  where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Aff (launchAff, delay)
import Data.Time.Duration (Milliseconds (..))
import Buttons as Buttons
import Image as Image
import Options.Applicative as OA
import Control.Alt ((<|>))
import Data.Either (Either(..))
import RPiDisplay as RPiDisplay
import WindowedDisplay as WindowedDisplay

main :: Effect Unit
main = 
  let prefs = (OA.fullDesc <> OA.progDesc "EReader Software") 
   in join <<< OA.execParser <<< ((flip OA.info) prefs) $ ado
        mode <- OA.flag' Buttons.WindowedMode ( OA.long "windowed" ) <|> pure Buttons.RPiMode
        in do
              _ <- launchAff $ do
                  let im = Image.loadSizedPalettizedImage Image.screenWidthHalf Image.screenHeightHalf "assets/large-book-test-quarter.png"
                  let im2 = Image.concatH im im
                  let im3 = Image.concatV im2 im2
                  case mode of 
                    Buttons.RPiMode -> RPiDisplay.display im3
                    Buttons.WindowedMode -> WindowedDisplay.display im3
              log "📖"
              Buttons.loggingPipeline mode