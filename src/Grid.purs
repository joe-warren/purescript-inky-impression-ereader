module Grid where

import Prelude 

import Data.Semigroup.Foldable
import Data.Foldable

import Data.Semigroup.Traversable
import Data.Traversable
import Control.Monad.State (evalState, get, modify)
import Data.Array as Array
import Data.Maybe (Maybe (..))

data Horizontal a = Horizontal a a a

data Vertical a = Vertical a a a a

data Grid a = Grid (Vertical (Horizontal a))

derive instance functorHorizontal :: Functor Horizontal

derive instance functorVertical :: Functor Vertical 

derive instance functorGrid :: Functor Grid

instance Foldable1 Horizontal where 
   foldMap1 f (Horizontal x y z) = f x <> f y <> f z 
   foldr1 f v = foldr1Default f v
   foldl1 f v = foldl1Default f v

instance Foldable Horizontal where 
    foldMap = foldMap1
    foldr a = foldrDefault a
    foldl a = foldlDefault a

instance Foldable1 Vertical where 
   foldMap1 f (Vertical w x y z) = f w <> f x <> f y <> f z 
   foldr1  f v = foldr1Default f v
   foldl1 f v = foldl1Default f v

instance Foldable Vertical where 
    foldMap = foldMap1
    foldr a = foldrDefault a
    foldl a = foldlDefault a

instance Foldable1 Grid where
   foldMap1 f (Grid c) = foldMap1 (foldMap1 f) c
   foldr1 f v = foldr1Default f v
   foldl1 f v = foldl1Default f v

instance Foldable Grid where 
    foldMap = foldMap1
    foldr a = foldrDefault a
    foldl a = foldlDefault a
   
instance Traversable1 Horizontal where
    traverse1 f (Horizontal x y z) = Horizontal <$> f x <*> f y <*> f z
    sequence1 f = sequence1Default f

instance Traversable Horizontal where
    traverse = traverse1
    sequence f = sequenceDefault f


instance Traversable1 Vertical where
    traverse1 f (Vertical w x y z) = Vertical <$> f w <*> f x <*> f y <*> f z
    sequence1 f = sequence1Default f

instance Traversable Vertical where
    traverse = traverse1
    sequence f = sequenceDefault f

instance Traversable1 Grid where
    traverse1 f (Grid h) = Grid <$> traverse1 (traverse1 f) h
    sequence1 f = sequence1Default f

instance Traversable Grid where
    traverse = traverse1
    sequence f = sequenceDefault f

instance Apply Horizontal where 
    apply (Horizontal fx fy fz) (Horizontal x y z) = Horizontal (fx x) (fy y) (fz z)


instance Apply Vertical where 
    apply (Vertical fw fx fy fz) (Vertical w x y z) = Vertical (fw w) (fx x) (fy y) (fz z)

instance Apply Grid where
    apply (Grid fh) (Grid h) =Grid ((<*>) <$> fh <*> h)

instance Applicative Horizontal where
    pure x = Horizontal x x x

    
instance Applicative Vertical where
    pure x = Vertical x x x x

instance Applicative Grid where
    pure = Grid <<< pure <<< pure

cellNumbers :: Grid Int
cellNumbers = evalState (traverse (const (get <* modify (_ + 1))) (pure unit)) 0

fromArray :: forall a. Array a -> Grid (Maybe a)
fromArray a = Array.index a <$> cellNumbers

