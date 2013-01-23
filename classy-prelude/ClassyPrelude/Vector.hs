{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.Vector
    ( Vector
    ) where

import qualified Prelude
import Prelude ((.), ($))
import ClassyPrelude.Classes
import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Monoid (Monoid)
import qualified Data.Monoid as Monoid
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable

instance CanMapFunc (Vector a) (Vector b) a b where
    mapFunc = V.map
instance CanConcatMapFunc (Vector a) (Vector b) a (Vector b) where
    concatMapFunc = V.concatMap
instance CanFilterFunc (Vector a) (Vector a) a where
    filterFunc = V.filter
instance CanFilterMFunc (Vector a) a where
    filterMFunc = V.filterM
instance CanLength (Vector a) Prelude.Int where
    length = V.length
instance CanSingleton (Vector a) a where
    singleton = V.singleton
instance CanNull (Vector a) where
    null = V.null
instance CanPack (Vector a) a where
    pack = V.fromList
    unpack = V.toList
instance CanIntersperse (Vector a) a where
    -- | Implementation is a rip off from <http://hackage.haskell.org/packages/archive/base/latest/doc/html/src/Data-List.html#intersperse>.
    intersperse _ xs | null xs = V.empty
    intersperse sep xs = V.cons (V.head xs) $ prependToAll sep $ V.unsafeTail xs
      where
        prependToAll _ xs | null xs = V.empty
        prependToAll sep xs = V.cons sep $ V.cons (V.head xs) $ prependToAll sep $ V.unsafeTail xs
instance Prelude.Monad m => CanMapMFunc (Vector i) (m (Vector o)) m i o where
    mapMFunc = V.mapM
instance CanMapM_Func (Vector a) a where
    mapM_Func = V.mapM_
instance Prelude.Eq x => CanMember (Vector x) x where
    member x = V.any (Prelude.== x)
instance CanBreak (Vector a) a where
    break = V.break
    span = V.span
    dropWhile = V.dropWhile
    takeWhile = V.takeWhile
instance CanAny (Vector a) a where
    any = V.any
    all = V.all
instance CanSplitAt (Vector a) Prelude.Int where
    splitAt = V.splitAt
instance CanFoldFunc (Vector a) a accum where
    foldFunc = V.foldl'

instance CanReverse (Vector a) where
    reverse = V.reverse

instance CanReplicate (Vector a) a Prelude.Int where
    replicate = V.replicate

instance CanReplicateM (Vector a) a Prelude.Int where
    replicateM = V.replicateM

instance CanFind (Vector a) a where
    find = V.find
    
instance (Monoid m) => CanConcat (Vector m) m where
    concat = Foldable.fold

instance CanPartition (Vector a) a where
    partition = V.partition
