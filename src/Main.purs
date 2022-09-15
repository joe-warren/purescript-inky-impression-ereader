module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Buttons as Buttons

main :: Effect Unit
main = do
  log "🍝"
  --Buttons.runButtons buttonCallback
  Buttons.loggingPipeline


buttonCallback :: Buttons.ButtonId -> Boolean -> Effect Unit
buttonCallback Buttons.Button1 true = log "1 Down"
buttonCallback Buttons.Button2 true = log "2 Down"
buttonCallback Buttons.Button3 true = log "3 Down"
buttonCallback Buttons.Button4 true = log "4 Down"
buttonCallback Buttons.Button1 false = log "1 Up"
buttonCallback Buttons.Button2 false = log "2 Up"
buttonCallback Buttons.Button3 false = log "3 Up"
buttonCallback Buttons.Button4 false = log "4 Up"