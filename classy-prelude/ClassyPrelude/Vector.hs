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

instance CanGroupBy (Vector a) a where
    -- | Implementation is stolen from <http://hackage.haskell.org/packages/archive/storablevector/latest/doc/html/src/Data-StorableVector.html#groupBy>
    groupBy k xs =
      switchL []
        (\h t ->
          let n = 1 + findIndexOrEnd (not . k h) t
          in  Vector.unsafeTake n xs : groupBy k (Vector.unsafeDrop n xs))
        xs

instance Eq a => CanGroup (Vector a) a where
    -- | A special case of 'groupBy', which is about 40% faster than 
    -- /groupBy (==)/.
    -- 
    -- Implementation is stolen from <http://hackage.haskell.org/packages/archive/storablevector/latest/doc/html/src/Data-StorableVector.html#groupBy>
    group xs =
      switchL []
        (\h _ ->
          let (ys, zs) = span (== h) xs
          in  ys : group zs)
        xs

-- Helper functions stolen from <http://hackage.haskell.org/packages/archive/storablevector/latest/doc/html/src/Data-StorableVector.html#groupBy>

-- | 'findIndexOrEnd' is a variant of findIndex, that returns the length
-- of the string if no element is found, rather than Nothing.
findIndexOrEnd p xs =
  Vector.foldr
    (\x k n ->
       if p x then n else k (succ n))
    id xs 0
switchL n j x =
  if null x
    then n
    else j (Vector.unsafeHead x) (Vector.unsafeTail x)


instance CanZipWith (Vector a) a (Vector b) b (Vector c) c where
    zipWith = Vector.zipWith

instance CanZipWith3 (Vector a) a (Vector b) b (Vector c) c (Vector d) d where
    zipWith3 = Vector.zipWith3

instance CanZipWith4 (Vector a) a (Vector b) b (Vector c) c (Vector d) d (Vector e) e where
    zipWith4 = Vector.zipWith4

instance CanZipWith5 (Vector a) a (Vector b) b (Vector c) c (Vector d) d (Vector e) e (Vector f) f where
    zipWith5 = Vector.zipWith5

instance CanZipWith6 (Vector a) a (Vector b) b (Vector c) c (Vector d) d (Vector e) e (Vector f) f (Vector g) g where
    zipWith6 = Vector.zipWith6

instance CanZip (Vector a) a (Vector b) b Vector where
    zip = Vector.zip

instance CanZip3 (Vector a) a (Vector b) b (Vector c) c Vector where
    zip3 = Vector.zip3

instance CanZip4 (Vector a) a (Vector b) b (Vector c) c (Vector d) d Vector where
    zip4 = Vector.zip4

instance CanZip5 (Vector a) a (Vector b) b (Vector c) c (Vector d) d (Vector e) e Vector where
    zip5 = Vector.zip5

instance CanZip6 (Vector a) a (Vector b) b (Vector c) c (Vector d) d (Vector e) e (Vector f) f Vector where
    zip6 = Vector.zip6

instance CanUnzip (Vector a) a (Vector b) b Vector where
    unzip = Vector.unzip

instance CanUnzip3 (Vector a) a (Vector b) b (Vector c) c Vector where
    unzip3 = Vector.unzip3

instance CanUnzip4 (Vector a) a (Vector b) b (Vector c) c (Vector d) d Vector where
    unzip4 = Vector.unzip4

instance CanUnzip5 (Vector a) a (Vector b) b (Vector c) c (Vector d) d (Vector e) e Vector where
    unzip5 = Vector.unzip5

instance CanUnzip6 (Vector a) a (Vector b) b (Vector c) c (Vector d) d (Vector e) e (Vector f) f Vector where
    unzip6 = Vector.unzip6
