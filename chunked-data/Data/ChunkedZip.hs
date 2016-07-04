-- | Various zipping and unzipping functions for chunked data structures.
module Data.ChunkedZip where

import Prelude hiding (zipWith, zipWith3)
import Control.Arrow ((&&&), (***))
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Vector as Vector
-- import qualified Data.Vector.Unboxed as UVector
import qualified Data.Sequence as Seq
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import qualified Data.IntMap as IntMap
import Data.Tree
import Data.Functor.Compose
import Data.Foldable (toList)

class Functor f => Zip f where
    zipWith :: (a -> b -> c) -> f a -> f b -> f c

    zip :: f a -> f b -> f (a, b)
    zip = zipWith (,)

    zap :: f (a -> b) -> f a -> f b
    zap = zipWith id

    unzip :: f (a, b) -> (f a, f b)
    unzip = fmap fst &&& fmap snd

instance Zip [] where
    zip = List.zip
    zipWith = List.zipWith
    unzip = List.unzip
instance Zip NonEmpty where
    zipWith = NonEmpty.zipWith
    zip = NonEmpty.zip
    unzip = NonEmpty.unzip
instance Zip Seq.Seq where
    zip = Seq.zip
    zipWith = Seq.zipWith
    unzip = (Seq.fromList *** Seq.fromList) . List.unzip . toList
instance Zip Tree where
    zipWith f (Node a as) (Node b bs) = Node (f a b) (zipWith (zipWith f) as bs)
instance Zip Vector.Vector where
    zip = Vector.zip
    unzip = Vector.unzip
    zipWith = Vector.zipWith
  {-
instance Zip UVector where
    zip = UVector.zip
    unzip = UVector.unzip
    zipWith = UVector.zipWith
    -}

instance Zip m => Zip (IdentityT m) where
    zipWith f (IdentityT m) (IdentityT n) = IdentityT (zipWith f m n)
instance Zip ((->)a) where
    zipWith f g h a = f (g a) (h a)
instance Zip m => Zip (ReaderT e m) where
    zipWith f (ReaderT m) (ReaderT n) = ReaderT $ \a ->
      zipWith f (m a) (n a)
instance Zip IntMap.IntMap where
    zipWith = IntMap.intersectionWith
instance (Zip f, Zip g) => Zip (Compose f g) where
    zipWith f (Compose a) (Compose b) = Compose $ zipWith (zipWith f) a b

class Functor f => Zip3 f where
    zipWith3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d

    zip3 :: f a -> f b -> f c -> f (a, b, c)
    zip3 = zipWith3 (\x y z -> (x,y,z))

    zap3 :: f (a -> b -> c) -> f a -> f b -> f c
    zap3 = zipWith3 id

    unzip3 :: f (a, b, c) -> (f a, f b, f c)
    -- unzip3 = fmap (\(x,_,_)->x) &&& fmap (\(_,x,_)->x) &&& fmap (\(_,_,x)->x)

instance Zip3 [] where
    zip3 = List.zip3
    unzip3 = List.unzip3
    zipWith3 = List.zipWith3
instance Zip3 Vector.Vector where
    zip3 = Vector.zip3
    unzip3 = Vector.unzip3
    zipWith3 = Vector.zipWith3
instance Zip3 Seq.Seq where
    zip3 = Seq.zip3
    unzip3 = (\(a, b, c) -> (Seq.fromList a, Seq.fromList b, Seq.fromList c)) . List.unzip3 . toList
    zipWith3 = Seq.zipWith3

class Functor f => Zip4 f where
    zipWith4 :: (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e

    zip4 :: f a -> f b -> f c -> f d ->  f (a, b, c, d)
    zip4 = zipWith4 (\w x y z -> (w, x,y,z))

    zap4 :: f (a -> b -> c -> d) -> f a -> f b -> f c -> f d
    zap4 = zipWith4 id

    unzip4 :: f (a, b, c, d) -> (f a, f b, f c, f d)

instance Zip4 [] where
    zip4 = List.zip4
    unzip4 = List.unzip4
    zipWith4 = List.zipWith4
instance Zip4 Vector.Vector where
    zip4 = Vector.zip4
    unzip4 = Vector.unzip4
    zipWith4 = Vector.zipWith4
instance Zip4 Seq.Seq where
    zip4 = Seq.zip4
    unzip4 = (\(a, b, c, d) -> (Seq.fromList a, Seq.fromList b, Seq.fromList c, Seq.fromList d)) . List.unzip4 . toList
    zipWith4 = Seq.zipWith4

class Functor f => Zip5 f where
    zipWith5 :: (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g

    zip5 :: f a -> f b -> f c -> f d -> f e -> f (a, b, c, d, e)
    zip5 = zipWith5 (\v w x y z -> (v,w,x,y,z))

    zap5 :: f (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
    zap5 = zipWith5 id

    unzip5 :: f (a, b, c, d, e) -> (f a, f b, f c, f d, f e)

instance Zip5 [] where
    zip5 = List.zip5
    unzip5 = List.unzip5
    zipWith5 = List.zipWith5
instance Zip5 Vector.Vector where
    zip5 = Vector.zip5
    unzip5 = Vector.unzip5
    zipWith5 = Vector.zipWith5

class Functor f => Zip6 f where
    zipWith6 :: (a -> b -> c -> d -> e -> g -> h) -> f a -> f b -> f c -> f d -> f e -> f g -> f h

    zip6 :: f a -> f b -> f c -> f d -> f e -> f g -> f (a, b, c, d, e, g)
    zip6 = zipWith6 (\u v w x y z -> (u, v,w,x,y,z))

    zap6 :: f (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g
    zap6 = zipWith6 id

    unzip6 :: f (a, b, c, d, e, g) -> (f a, f b, f c, f d, f e, f g)

instance Zip6 [] where
    zip6 = List.zip6
    unzip6 = List.unzip6
    zipWith6 = List.zipWith6
instance Zip6 Vector.Vector where
    zip6 = Vector.zip6
    unzip6 = Vector.unzip6
    zipWith6 = Vector.zipWith6

class Functor f => Zip7 f where
    zipWith7 :: (a -> b -> c -> d -> e -> g -> h -> i) -> f a -> f b -> f c -> f d -> f e -> f g -> f h -> f i

    zip7 :: f a -> f b -> f c -> f d -> f e -> f g -> f h -> f (a, b, c, d, e, g, h)
    zip7 = zipWith7 (\t u v w x y z -> (t,u,v,w,x,y,z))

    zap7 :: f (a -> b -> c -> d -> e -> g -> h) -> f a -> f b -> f c -> f d -> f e -> f g -> f h
    zap7 = zipWith7 id

    unzip7 :: f (a, b, c, d, e, g, h) -> (f a, f b, f c, f d, f e, f g, f h)
    -- unzip3 = fmap (\(x,_,_)->x) &&& fmap (\(_,x,_)->x) &&& fmap (\(_,_,x)->x)

instance Zip7 [] where
    zip7 = List.zip7
    unzip7 = List.unzip7
    zipWith7 = List.zipWith7
