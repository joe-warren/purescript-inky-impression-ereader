module Main
  ( main
  )
  where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Buttons as Buttons

import Options.Applicative as OA
import Control.Alt ((<|>))

main :: Effect Unit
main = 
  let prefs = (OA.fullDesc <> OA.progDesc "EReader Software") 
   in join <<< OA.execParser <<< ((flip OA.info) prefs) $ ado
        mode <- OA.flag' Buttons.WindowedMode ( OA.long "windowed" ) <|> pure Buttons.RPiMode
        in do
              log "📖"
              Buttons.loggingPipeline mode