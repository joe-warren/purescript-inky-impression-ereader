module Streams
  ( (>->)
  , Stream(..)
  , await
  , chain
  , concurrently
  , consumer
  , drain
  , inChannels
  , logShowStream
  , producer
  , runStream
  , sApp
  , sFilter
  , sMap
  , statefully
  , yield
  )
  where

import Prelude

import Control.Monad.Reader.Trans (ReaderT, ask, lift, runReaderT)

import Data.Tuple (Tuple (..), fst, snd)

import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff (never)
import Effect.Aff.AVar as AVar
import Effect.Class.Console (logShow)
import Effect.Class (class MonadEffect)

import Control.Parallel.Class (class Parallel, sequential, parallel)
import Data.Traversable (sequence, traverse)
import Data.Foldable (class Foldable, fold, traverse_)
import Data.List as List
import Control.Monad.Trans.Class(class MonadTrans)

newtype Stream i o m a = Stream (ReaderT (Tuple (m i) (o -> m Unit)) m a)

rawStream :: forall i o m a. Stream i o m a -> (ReaderT (Tuple (m i) (o -> m Unit)) m a)
rawStream (Stream s) = s

runStream :: forall i o m a. MonadAff m => Stream i o m a -> m a
runStream = runRaw (liftAff never) (const (pure unit))

runRaw :: forall m i o a. m i -> (o -> m Unit) -> Stream i o m a -> m a
runRaw aw ye s = runReaderT (rawStream s) (Tuple aw ye)

await :: forall m i o. Monad m => Stream i o m i
await = Stream $ join (lift <<< fst <$> ask)

yield :: forall m i o. Monad m => o -> Stream i o m Unit
yield o = Stream $ join ( (lift <<< (_ $ o) <<< snd) <$> ask )

producer :: forall m i o a. (MonadAff m ) => ((o -> m Unit) -> m a) -> Stream i o m a
producer fa = Stream $ do
    Tuple _ yieldOut <- ask
    lift $ fa yieldOut

consumer :: forall m i o a. (MonadAff m ) => (m i -> m a) -> Stream i o m a
consumer fa = Stream $ do
    Tuple awaitIn _ <- ask
    lift $ fa awaitIn

drain :: forall m i o a. Monad m => Stream i o m a
drain = do
   _ <- await
   drain

sMap :: forall m i o a. Monad m => (i -> o) -> Stream i o m a
sMap f = sApp (pure <<< f)

sApp :: forall m i o a. Monad m => (i -> m o) -> Stream i o m a
sApp f =
  let go = do
              i <- await
              o <- Stream <<< lift $ f i
              yield o
              go
  in go

sFilter :: forall m i a. Monad m => (i -> Boolean) -> Stream i i m a
sFilter f = 
  let go = do
              i <- await
              when (f i) (yield i)
              go
  in go

logShowStream :: forall m i a. (MonadEffect m) => (Show i) => Stream i i m a
logShowStream = sApp (\v -> v <$ logShow v ) 

chain :: forall m f i e o a. (Monad m) => (MonadAff m) => (Parallel f m) => (Semigroup a) => Stream i e m a -> Stream e o m a -> Stream i o m a  
chain inp out = Stream $ do 
    av <- lift <<< liftAff $ AVar.empty
    Tuple awaitIn yieldOut <- ask
    sequential $ append <$>
        (parallel <<< lift $ runRaw awaitIn (\o -> liftAff $ AVar.put o av) inp) <*>
        (parallel <<< lift $ runRaw (liftAff $ AVar.take av) yieldOut out)


concurrently :: forall m f i o a t. Monad m => MonadAff m => Parallel f m => Foldable t => Monoid a => t (Stream i o m a) -> Stream i o m a
concurrently vs = Stream $ do
    let vvs = List.fromFoldable vs
    Tuple awaitIn yieldOut <- ask
    withAvars <- lift $ traverse (sequence <<< (flip Tuple $ liftAff AVar.empty)) vvs
    let avars = snd <$> withAvars
    let push = do
                  v <- awaitIn
                  liftAff $ traverse_ (AVar.put v) avars
                  push
    let runOne (Tuple s av) = parallel $ runRaw (liftAff $ AVar.take av) yieldOut s
    lift <<< sequential $ (parallel push *> (fold <$> traverse runOne withAvars))

inChannels :: forall m f i o c a t. Monad m => MonadAff m => Parallel f m => Monoid a => Functor t => Foldable t => Eq c => t c -> Stream i o m a -> Stream (Tuple c i) (Tuple c o) m a
inChannels cs s = let oneChannel c = sFilter ((_ == c) <<< fst) >-> sMap snd >-> s >-> sMap (Tuple c)
                   in concurrently (oneChannel <$> cs)

statefully :: forall m i s a. Monad m => (i -> s -> m s) -> s -> Stream i s m a
statefully f = 
  let go st = do
                  yield st
                  e <- await 
                  st' <- lift $ f e st
                  go st'
  in go

infixr 5 chain as >->
        
derive newtype instance Functor m => Functor (Stream i o m)
derive newtype instance Apply m => Apply (Stream i o m)
derive newtype instance Applicative m => Applicative (Stream i o m)
derive newtype instance Bind m => Bind (Stream i o m)
derive newtype instance Monad m => Monad (Stream i o m)
instance MonadTrans (Stream i o) where 
  lift = Stream <<< lift

derive newtype instance MonadEffect m => MonadEffect (Stream i o m)

derive newtype instance MonadAff m => MonadAff (Stream i o m)
