module Streams
  ( (>->)
  , Stream(..)
  , await
  , chain
  , concurrently
  , consumer
  , drain
  , logShowStream
  , producer
  , runStream
  , sApp
  , sFilter
  , sMap
  , yield
  )
  where

import Prelude

import Control.Monad.Reader.Trans

import Data.Tuple (Tuple (..), fst, snd)

import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff (Aff, Error, delay, forkAff, never)
import Effect.Aff.AVar as AVar
import Effect.Class.Console (logShow)
import Effect.Class (class MonadEffect)

import Control.Parallel.Class (class Parallel, sequential, parallel)
import Data.Traversable (sequence, traverse)
import Data.Profunctor.Strong ((&&&))
import Data.Foldable (class Foldable, fold, foldMap, traverse_)
import Data.List as List

import Data.Semigroup (append)
newtype Stream m i o a = Stream (ReaderT (Tuple (m i) (o -> m Unit)) m a)

rawStream :: forall m i o a. Stream m i o a -> (ReaderT (Tuple (m i) (o -> m Unit)) m a)
rawStream (Stream s) = s

runStream :: forall m i o a. MonadAff m => Stream m i o a -> m a
runStream = runRaw (liftAff never) (const (pure unit))

runRaw :: forall m i o a. m i -> (o -> m Unit) -> Stream m i o a -> m a
runRaw aw ye s = runReaderT (rawStream s) (Tuple aw ye)

await :: forall m i o. Monad m => Stream m i o i
await = Stream $ join (lift <<< fst <$> ask)

yield :: forall m i o. Monad m => o -> Stream m i o Unit
yield o = Stream $ join ( (lift <<< (_ $ o) <<< snd) <$> ask )

producer :: forall m i o a. (MonadAff m ) => ((o -> m Unit) -> m a) -> Stream m i o a
producer fa = Stream $ do
    Tuple _ yieldOut <- ask
    lift $ fa yieldOut

consumer :: forall m i o a. (MonadAff m ) => (m i -> m a) -> Stream m i o a
consumer fa = Stream $ do
    Tuple awaitIn _ <- ask
    lift $ fa awaitIn

drain :: forall m i o a. Monad m => Stream m i o a
drain = do
   _ <- await
   drain

sMap :: forall m i o a. Monad m => (i -> o) -> Stream m i o a
sMap f = sApp (pure <<< f)

sApp :: forall m i o a. Monad m => (i -> m o) -> Stream m i o a
sApp f =
  let go = do
              i <- await
              o <- Stream <<< lift $ f i
              yield o
              go
  in go

sFilter :: forall m i a. Monad m => (i -> Boolean) -> Stream m i i a
sFilter f = 
  let go = do
              i <- await
              when (f i) (yield i)
              go
  in go

logShowStream :: forall m i a. (MonadEffect m) => (Show i) => Stream m i i a
logShowStream = sApp (\v -> v <$ logShow v ) 

chain :: forall m f i e o a. (Monad m) => (MonadAff m) => (Parallel f m) => (Monoid a) => Stream m i e a -> Stream m e o a -> Stream m i o a  
chain inp out = Stream $ do 
    av <- lift <<< liftAff $ AVar.empty
    Tuple awaitIn yieldOut <- ask
    --r1 <- lift $ runReaderT (runStream inp) (Tuple awaitIn (\o -> liftAff $ AVar.put o av)) 
    --r2 <- lift $ runReaderT (runStream out) (Tuple (liftAff $ AVar.take av) yieldOut)
    sequential $ append <$>
        (parallel <<< lift $ runRaw awaitIn (\o -> liftAff $ AVar.put o av) inp) <*>
        (parallel <<< lift $ runRaw (liftAff $ AVar.take av) yieldOut out)


concurrently ::forall m f i o a t. Monad m => MonadAff m => Parallel f m => Foldable t => Monoid a => t (Stream m i o a) -> Stream m i o a
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

infixr 5 chain as >->
        
derive newtype instance Functor m => Functor (Stream m i o)
derive newtype instance Apply m => Apply (Stream m i o)
derive newtype instance Applicative m => Applicative (Stream m i o)
derive newtype instance Bind m => Bind (Stream m i o)
derive newtype instance Monad m => Monad (Stream m i o)

derive newtype instance MonadEffect m => MonadEffect (Stream m i o)

derive newtype instance MonadAff m => MonadAff (Stream m i o)
