
module Buttons
  ( ButtonId(..)
  , runButtons
  )
  where

import Prelude
import Effect (Effect)
import Effect.Aff (runAff, launchAff_)
import Control.Promise (Promise, toAffE)
import Effect.Console (log)

data ButtonId = Button1 | Button2 | Button3 | Button4

foreign import runButtonsRaw :: (Int -> Effect Unit) -> Effect (Promise Unit)

buttonNum :: ButtonId -> Int
buttonNum Button1 = 1
buttonNum Button2 = 2
buttonNum Button3 = 3
buttonNum Button4 = 4

numToButton :: Int -> ButtonId
numToButton 1 = Button1
numToButton 2 = Button2
numToButton 3 = Button3 
numToButton 4 = Button4
numToButton _ = Button1

runButtons :: (ButtonId -> Effect Unit) -> Effect Unit
runButtons f = launchAff_ (toAffE (runButtonsRaw (numToButton >>> f)))
