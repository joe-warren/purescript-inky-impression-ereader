module Main
  ( main
  )
  where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Aff (launchAff)
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
                  im <- Image.loadSizedPalettizedImage Image.screenWidth Image.screenHeight "assets/large-book-test.png"
                  case im of 
                    Left str -> liftEffect $ log str
                    Right img -> do
                        liftEffect $ log "loading image"
                        case mode of 
                          Buttons.RPiMode -> RPiDisplay.display img 
                          Buttons.WindowedMode -> WindowedDisplay.display img
              log "📖"
              Buttons.loggingPipeline mode