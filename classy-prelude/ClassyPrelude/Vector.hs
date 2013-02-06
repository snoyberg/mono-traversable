{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.Vector
    ( Vector
    ) where

import Prelude ()
import CorePrelude
import ClassyPrelude.Classes
import qualified Data.Foldable as Foldable
import qualified Data.Vector as Vector


instance CanMapFunc (Vector a) (Vector b) a b where
    mapFunc = Vector.map

instance CanConcatMapFunc (Vector a) (Vector b) a (Vector b) where
    concatMapFunc = Vector.concatMap

instance CanFilterFunc (Vector a) (Vector a) a where
    filterFunc = Vector.filter

instance CanFilterMFunc (Vector a) a where
    filterMFunc = Vector.filterM

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

instance Monad m => CanMapMFunc (Vector i) (m (Vector o)) m i o where
    mapMFunc = Vector.mapM

instance CanMapM_Func (Vector a) a where
    mapM_Func = Vector.mapM_

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

instance CanFoldFunc (Vector a) a accum where
    foldFunc = Vector.foldl'

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
