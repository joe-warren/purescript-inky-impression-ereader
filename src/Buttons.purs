
module Buttons
  ( ButtonId(..)
  , runButtons
  , loggingPipeline
  )
  where

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Prelude
import Effect (Effect)
import Effect.Aff (Aff, runAff, launchAff_, delay, error)
import Effect.Aff.Class (liftAff)
import Control.Promise (Promise, toAffE)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Control.Applicative (when, unless)
import Control.Monad (join)
import Control.Parallel.Class (sequential, parallel)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe (..), isJust)
import Effect.Aff.AVar as AVar

import Pipes hiding (discard)
import Pipes.Prelude as P
import Pipes.Core (Producer, Pipe, runEffect)
import Pipes.Aff (send, spawn, split, unbounded, fromInput, input, toOutput, recv, new, realTime, seal, kill)
import Data.Tuple
import Effect.Now (nowDateTime)
import Data.DateTime as DateTime
import Data.Time.Duration (Milliseconds (..))

data ButtonId = Button1 | Button2 | Button3 | Button4 

derive instance genericButtonId :: Generic ButtonId _

instance showButtonId :: Show ButtonId where
  show = genericShow

data PressType = ShortTap | LongTap | DoubleTap

derive instance genericPressType :: Generic PressType _

instance showPressType :: Show PressType where
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


---perButton :: forall a b m. Monad m => Pipe a b m Unit -> Pipe (Tuple ButtonId a) (Tuple ButtonId b) m Unit
---perButton = ???

timeOut :: forall a b. Show a => Milliseconds -> Pipe a b Aff (Maybe a)
timeOut t = do
  av <- liftAff  AVar.empty
  chan <- liftAff (spawn $ new)
  liftEffect $ launchAff_  do
      vv <- map join $ sequential $ oneOf
              [ parallel $ Nothing <$ (delay t)
              , parallel $ (Just <$> recv chan)
              ]
      AVar.put vv av
      seal chan
  P.take 1 >-> toOutput chan
  liftAff $ AVar.read av 

clickProcessor :: Pipe Boolean PressType Aff Unit
clickProcessor = do
  c1 <- await 
  t1 <- liftEffect nowDateTime
  when c1 $ do
    c2 <- await
    unless c2 $ do
      t2 <- liftEffect nowDateTime
      if (DateTime.diff t2 t1 > (500.0 # Milliseconds))
         then yield LongTap 
         else do 
            mc3 <- timeOut (500.0 # Milliseconds)
            case mc3 of
              Nothing -> do
                 liftEffect $ log "s"
                 yield ShortTap
              Just false -> liftEffect $ log "here"
              Just true -> do 
                  liftEffect $ log "d"
                  c4 <- await
                  liftEffect $ log $ show c4
                  unless c4 $ yield DoubleTap 
  clickProcessor
        
loggingAction = rawProducer >-> P.map snd >-> clickProcessor >-> P.show >-> P.chain (liftEffect <<< log) >-> P.drain

loggingPipeline = launchAff_ (runEffect loggingAction)

