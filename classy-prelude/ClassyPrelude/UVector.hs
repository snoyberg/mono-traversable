{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.UVector
    ( UVector
    ) where

import CorePrelude
import ClassyPrelude.Classes
import qualified Data.Vector.Unboxed as UVector

instance (Unbox a, Unbox b) => CanMap (UVector a) (UVector b) a b where
    map = UVector.map

instance (Unbox a, Unbox b) => CanConcatMap (UVector a) (UVector b) a (UVector b) where
    concatMap = UVector.concatMap

instance (Unbox a) => CanFilter (UVector a) a where
    filter = UVector.filter

instance (Unbox a) => CanFilterM (UVector a) a where
    filterM = UVector.filterM

instance (Unbox a) => CanLength (UVector a) Int where
    length = UVector.length

instance (Unbox a) => CanSingleton (UVector a) a where
    singleton = UVector.singleton

instance (Unbox a) => CanNull (UVector a) where
    null = UVector.null

instance (Unbox a) => CanPack (UVector a) a where
    pack = UVector.fromList
    unpack = UVector.toList

instance (Unbox a) => CanIntersperse (UVector a) a where
    -- | Implementation is a rip off from <http://hackage.haskell.org/packages/archive/base/latest/doc/html/src/Data-List.html#intersperse>.
    intersperse _ xs | null xs = UVector.empty
    intersperse sep xs = UVector.cons (UVector.head xs) $ prependToAll sep $ UVector.unsafeTail xs
      where
        prependToAll _ xs | null xs = UVector.empty
        prependToAll sep xs = UVector.cons sep $ UVector.cons (UVector.head xs) $ prependToAll sep $ UVector.unsafeTail xs

instance (Unbox a, Eq a) => CanStripPrefix (UVector a) where
    stripPrefix x y
        | x == y1 = Just y2
        | otherwise = Nothing
      where
        (y1, y2) = UVector.splitAt (UVector.length x) y
    isPrefixOf x y = UVector.take (UVector.length x) y == x

instance (Unbox i, Unbox o, Monad m) => CanMapM (UVector i) (m (UVector o)) m i o where
    mapM = UVector.mapM

instance (Unbox a) => CanMapM_ (UVector a) a where
    mapM_ = UVector.mapM_

instance (Unbox x, Eq x) => CanMember (UVector x) x where
    member x = UVector.any (== x)

instance (Unbox a) => CanBreak (UVector a) a where
    break = UVector.break
    span = UVector.span
    dropWhile = UVector.dropWhile
    takeWhile = UVector.takeWhile

instance (Unbox a) => CanAny (UVector a) a where
    any = UVector.any
    all = UVector.all

instance (Unbox a) => CanSplitAt (UVector a) Int where
    splitAt = UVector.splitAt

instance (Unbox a) => CanFold (UVector a) a accum where
    fold = UVector.foldl'

instance (Unbox a) => CanReverse (UVector a) where
    reverse = UVector.reverse

instance (Unbox a) => CanReplicate (UVector a) a Int where
    replicate = UVector.replicate

instance (Unbox a) => CanReplicateM (UVector a) a Int where
    replicateM = UVector.replicateM

instance (Unbox a) => CanFind (UVector a) a where
    find = UVector.find
    
instance (Unbox m, Monoid m) => CanConcat (UVector m) m where
    concat = UVector.foldr mappend mempty

instance (Unbox a) => CanPartition (UVector a) a where
    partition = UVector.partition

instance (Unbox a) => CanCons (UVector a) a where
    cons = UVector.cons

instance (Unbox a) => CanUncons (UVector a) a where
    uncons v = if null v then Nothing else Just (UVector.unsafeHead v, UVector.unsafeTail v)

instance (Unbox a) => CanGroupBy (UVector a) a where
    -- | Implementation is stolen from <http://hackage.haskell.org/packages/archive/storablevector/latest/doc/html/src/Data-StorableVector.html#groupBy>
    groupBy k xs =
      switchL []
        (\h t ->
          let n = 1 + findIndexOrEnd (not . k h) t
          in  UVector.unsafeTake n xs : groupBy k (UVector.unsafeDrop n xs))
        xs

instance (Unbox a, Eq a) => CanGroup (UVector a) a where
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
  UVector.foldr
    (\x k n ->
       if p x then n else k (succ n))
    id xs 0
switchL n j x =
  if null x
    then n
    else j (UVector.unsafeHead x) (UVector.unsafeTail x)


instance (Unbox a, Unbox b, Unbox c) => CanZipWith (UVector a) a (UVector b) b (UVector c) c where
    zipWith = UVector.zipWith

instance (Unbox a, Unbox b, Unbox c, Unbox d) => CanZipWith3 (UVector a) a (UVector b) b (UVector c) c (UVector d) d where
    zipWith3 = UVector.zipWith3

instance (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e) => CanZipWith4 (UVector a) a (UVector b) b (UVector c) c (UVector d) d (UVector e) e where
    zipWith4 = UVector.zipWith4

instance (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f) => CanZipWith5 (UVector a) a (UVector b) b (UVector c) c (UVector d) d (UVector e) e (UVector f) f where
    zipWith5 = UVector.zipWith5

instance (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f, Unbox g) => CanZipWith6 (UVector a) a (UVector b) b (UVector c) c (UVector d) d (UVector e) e (UVector f) f (UVector g) g where
    zipWith6 = UVector.zipWith6

instance (Unbox a, Unbox b) => CanZip (UVector a) a (UVector b) b UVector where
    zip = UVector.zip

instance (Unbox a, Unbox b, Unbox c) => CanZip3 (UVector a) a (UVector b) b (UVector c) c UVector where
    zip3 = UVector.zip3

instance (Unbox a, Unbox b, Unbox c, Unbox d) => CanZip4 (UVector a) a (UVector b) b (UVector c) c (UVector d) d UVector where
    zip4 = UVector.zip4

instance (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e) => CanZip5 (UVector a) a (UVector b) b (UVector c) c (UVector d) d (UVector e) e UVector where
    zip5 = UVector.zip5

instance (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f) => CanZip6 (UVector a) a (UVector b) b (UVector c) c (UVector d) d (UVector e) e (UVector f) f UVector where
    zip6 = UVector.zip6

instance (Unbox a, Unbox b) => CanUnzip (UVector a) a (UVector b) b UVector where
    unzip = UVector.unzip

instance (Unbox a, Unbox b, Unbox c) => CanUnzip3 (UVector a) a (UVector b) b (UVector c) c UVector where
    unzip3 = UVector.unzip3

instance (Unbox a, Unbox b, Unbox c, Unbox d) => CanUnzip4 (UVector a) a (UVector b) b (UVector c) c (UVector d) d UVector where
    unzip4 = UVector.unzip4

instance (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e) => CanUnzip5 (UVector a) a (UVector b) b (UVector c) c (UVector d) d (UVector e) e UVector where
    unzip5 = UVector.unzip5

instance (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f) => CanUnzip6 (UVector a) a (UVector b) b (UVector c) c (UVector d) d (UVector e) e (UVector f) f UVector where
    unzip6 = UVector.unzip6

instance (Unbox a) => CanEmpty (UVector a) where
    empty = UVector.empty
