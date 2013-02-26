{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.Vector
    ( Vector
    ) where

import CorePrelude
import ClassyPrelude.Classes
import qualified Data.Foldable as Foldable
import qualified Data.Vector as Vector


instance CanMap (Vector a) (Vector b) a b where
    map = Vector.map

instance CanConcatMap (Vector a) (Vector b) a (Vector b) where
    concatMap = Vector.concatMap

instance CanFilter (Vector a) a where
    filter = Vector.filter

instance CanFilterM (Vector a) a where
    filterM = Vector.filterM

instance CanLength (Vector a) Int where
    length = Vector.length

instance CanSingleton (Vector a) a where
    singleton = Vector.singleton

instance CanNull (Vector a) where
    null = Vector.null

instance CanPack (Vector a) a where
    pack = Vector.fromList
    unpack = Vector.toList

instance CanIntersperse (Vector a) a where
    -- | Implementation is a rip off from <http://hackage.haskell.org/packages/archive/base/latest/doc/html/src/Data-List.html#intersperse>.
    intersperse _ xs | null xs = Vector.empty
    intersperse sep xs = Vector.cons (Vector.head xs) $ prependToAll sep $ Vector.unsafeTail xs
      where
        prependToAll _ xs | null xs = Vector.empty
        prependToAll sep xs = Vector.cons sep $ Vector.cons (Vector.head xs) $ prependToAll sep $ Vector.unsafeTail xs

instance Eq a => CanStripPrefix (Vector a) where
    stripPrefix x y
        | x == y1 = Just y2
        | otherwise = Nothing
      where
        (y1, y2) = Vector.splitAt (Vector.length x) y
    isPrefixOf x y = Vector.take (Vector.length x) y == x

instance Monad m => CanMapM (Vector i) (m (Vector o)) m i o where
    mapM = Vector.mapM

instance CanMapM_ (Vector a) a where
    mapM_ = Vector.mapM_

instance Eq x => CanMember (Vector x) x where
    member x = Vector.any (== x)

instance CanBreak (Vector a) a where
    break = Vector.break
    span = Vector.span
    dropWhile = Vector.dropWhile
    takeWhile = Vector.takeWhile

instance CanAny (Vector a) a where
    any = Vector.any
    all = Vector.all

instance CanSplitAt (Vector a) Int where
    splitAt = Vector.splitAt

instance CanFold (Vector a) a accum where
    fold = Vector.foldl'

instance CanReverse (Vector a) where
    reverse = Vector.reverse

instance CanReplicate (Vector a) a Int where
    replicate = Vector.replicate

instance CanReplicateM (Vector a) a Int where
    replicateM = Vector.replicateM

instance CanFind (Vector a) a where
    find = Vector.find
    
instance (Monoid m) => CanConcat (Vector m) m where
    concat = Foldable.fold

instance CanPartition (Vector a) a where
    partition = Vector.partition

instance CanCons (Vector a) a where
    cons = Vector.cons

instance CanUncons (Vector a) a where
    uncons v = if null v then Nothing else Just (Vector.unsafeHead v, Vector.unsafeTail v)
