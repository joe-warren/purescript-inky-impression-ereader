
module Buttons
  ( runButtons,
    ButtonId (..),
    loggingPipeline
  )
  where

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Prelude
import Effect (Effect)
import Effect.Aff (Aff, runAff, launchAff_)
import Effect.Aff.Class (liftAff)
import Control.Promise (Promise, toAffE)
import Effect.Console (log)
import Effect.Class (liftEffect)


import Pipes hiding (discard)
import Pipes.Prelude as P
import Pipes.Core (Producer, runEffect)
import Pipes.Aff (send, spawn, split, unbounded, fromInput, input)
import Data.Tuple

data ButtonId = Button1 | Button2 | Button3 | Button4 

derive instance genericButtonId :: Generic ButtonId _

instance showButtonId :: Show ButtonId where
  show = genericShow

foreign import runButtonsRaw :: (Int -> Boolean -> Effect Unit) -> Effect (Promise Unit)

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

runButtons :: (ButtonId -> Boolean -> Effect Unit) -> Effect Unit
runButtons f = launchAff_ (toAffE (runButtonsRaw (numToButton >>> f)))

rawProducer :: Producer (Tuple ButtonId Boolean) Aff Unit
rawProducer = do
   chan <- liftAff (spawn unbounded)
   liftEffect (runButtons (\b p -> launchAff_ (void ( send (Tuple b p) chan))))
   fromInput chan


loggingAction = rawProducer >-> P.show >-> P.chain (liftEffect <<< log) >-> P.drain

loggingPipeline = launchAff_ (runEffect loggingAction)

