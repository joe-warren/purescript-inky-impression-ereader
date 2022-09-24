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

main :: Effect Unit
main = 
  let prefs = (OA.fullDesc <> OA.progDesc "EReader Software") 
   in join <<< OA.execParser <<< ((flip OA.info) prefs) $ ado
        mode <- OA.flag' Buttons.WindowedMode ( OA.long "windowed" ) <|> pure Buttons.RPiMode
        in do
              _ <- launchAff $ do
                  im <- Image.loadPalettizedImage "assets/large-book-test.png"
                  case im of 
                    Left str -> liftEffect $ log str
                    Right img -> liftEffect $ log "loaded image" 

              log "📖"
              --Buttons.loggingPipeline mode