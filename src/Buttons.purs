
module Buttons
  ( ButtonId(..)
  , Mode(..)
  , loggingPipeline
  )
  where

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Prelude
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, delay)
import Control.Promise as Promise
import Effect.Class (liftEffect)
import Control.Parallel.Class (sequential, parallel)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe (..))
import Streams as SR
import Streams ((>->))
import Data.Tuple (Tuple (..))
import Effect.Now (nowDateTime)
import Data.DateTime as DateTime
import Data.Time.Duration (Milliseconds (..))

import RawWindowedButtons as WindowedButtons
import RawRPiButtons as RPiButtons

data Mode = RPiMode | WindowedMode 

data ButtonId = Button1 | Button2 | Button3 | Button4 

derive instance Eq ButtonId

buttonIds :: Array ButtonId
buttonIds = [Button1, Button2, Button3, Button4]

derive instance genericButtonId :: Generic ButtonId _

instance showButtonId :: Show ButtonId where
  show = genericShow

data PressType = ShortTap | LongTap | DoubleTap

derive instance genericPressType :: Generic PressType _

instance showPressType :: Show PressType where
  show = genericShow

numToButton :: Int -> ButtonId
numToButton 1 = Button1
numToButton 2 = Button2
numToButton 3 = Button3 
numToButton 4 = Button4
numToButton _ = Button1

runButtonsRPi :: (ButtonId -> Boolean -> Effect Unit) -> Effect Unit
runButtonsRPi f = launchAff_ (Promise.toAffE (RPiButtons.runButtonsRaw (numToButton >>> f)))

runButtonsWindowed :: (ButtonId -> Boolean -> Effect Unit) -> Effect Unit
runButtonsWindowed f = launchAff_ (Promise.toAffE (WindowedButtons.runButtonsRaw (numToButton >>> f)))

rawProducer :: Mode -> SR.Stream Aff Void (Tuple ButtonId Boolean) Unit
rawProducer mode = 
  let runButtons = 
        case mode of 
          RPiMode -> runButtonsRPi
          WindowedMode -> runButtonsWindowed
  in SR.producer $ \send -> do
   liftEffect (runButtons (\b p -> launchAff_  ( send (Tuple b p))))

---perButton :: forall a b m. Monad m => Pipe a b m Unit -> Pipe (Tuple ButtonId a) (Tuple ButtonId b) m Unit
---perButton = ???

timeOut :: forall i o. Milliseconds -> SR.Stream Aff i o (Maybe i)
timeOut t = SR.consumer $ \await' -> do
        sequential $ oneOf
              [ parallel $ Nothing <$ (delay t)
              , parallel $ (Just <$> await')
              ]

clickProcessor :: SR.Stream Aff Boolean PressType Unit
clickProcessor = do
  c1 <- SR.await 
  t1 <- liftEffect nowDateTime
  when c1 $ do
    c2 <- SR.await
    unless c2 $ do
      t2 <- liftEffect nowDateTime
      if (DateTime.diff t2 t1 > (500.0 # Milliseconds))
         then SR.yield LongTap 
         else do 
            mc3 <- timeOut (500.0 # Milliseconds)
            case mc3 of
              Nothing -> do
                 SR.yield ShortTap
              Just false -> pure unit
              Just true -> do 
                  c4 <- SR.await
                  unless c4 $ SR.yield DoubleTap 
  clickProcessor
        
loggingPipeline :: Mode -> Effect Unit
loggingPipeline mode = 
  let stream = rawProducer mode >-> SR.inChannels buttonIds clickProcessor >-> SR.logShowStream >-> SR.drain
  in launchAff_ $ do
      (SR.runStream stream)

