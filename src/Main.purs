module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Buttons as Buttons

main :: Effect Unit
main = do
  log "🍝"
  Buttons.runButtons buttonCallback


buttonCallback :: Buttons.ButtonId -> Effect Unit
buttonCallback Buttons.Button1 = log "1"
buttonCallback Buttons.Button2 = log "2"
buttonCallback Buttons.Button3 = log "3"
buttonCallback Buttons.Button4 = log "4"