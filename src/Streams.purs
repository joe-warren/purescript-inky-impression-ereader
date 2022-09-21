module Streams
  ( Stream(..)
  , await
  , chain
  , yield
  )
  where

import Prelude

import Control.Monad.Reader.Trans

import Data.Tuple (Tuple (..), fst, snd)

import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff (Aff, Error, delay, forkAff)
import Effect.Aff.AVar as AVar


import Control.Parallel.Class (class Parallel, sequential, parallel)
import Data.Foldable (foldMap)

import Data.Semigroup (append)
newtype Stream m i o a = Stream (ReaderT (Tuple (m i) (o -> m Unit)) m a)

runStream :: forall m i o a. Stream m i o a -> (ReaderT (Tuple (m i) (o -> m Unit)) m a)
runStream (Stream s) = s

await :: forall m i o. Monad m => Stream m i o i
await = Stream $ join (lift <<< fst <$> ask)

yield :: forall m i o. Monad m => o -> Stream m i o Unit
yield o = Stream $ join ( (lift <<< (_ $ o) <<< snd) <$> ask )

chain :: forall m f i e o a. (Monad m) => (MonadAff m) => (Parallel f m) => (Monoid a) => Stream m i e a -> Stream m e o a -> Stream m i o a  
chain inp out = Stream $ do 
    av <- lift <<< liftAff $ AVar.empty
    Tuple awaitIn yieldOut <- ask
    --r1 <- lift $ runReaderT (runStream inp) (Tuple awaitIn (\o -> liftAff $ AVar.put o av)) 
    --r2 <- lift $ runReaderT (runStream out) (Tuple (liftAff $ AVar.take av) yieldOut)
    sequential $ append <$>
        (parallel <<< lift $ runReaderT (runStream inp) (Tuple awaitIn (\o -> liftAff $ AVar.put o av))) <*>
        (parallel <<< lift $ runReaderT (runStream out) (Tuple (liftAff $ AVar.take av) yieldOut))
derive newtype instance Functor m => Functor (Stream m i o)
derive newtype instance Apply m => Apply (Stream m i o)
derive newtype instance Applicative m => Applicative (Stream m i o)
derive newtype instance Bind m => Bind (Stream m i o)
derive newtype instance Monad m => Monad (Stream m i o)

