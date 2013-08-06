{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.List () where

import CorePrelude
import ClassyPrelude.Classes
import qualified Data.List as List
import qualified Control.Monad as Monad
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set


instance CanMap [a] [b] a b where
    map = List.map

instance CanConcatMap [a] [b] a [b] where
    concatMap = List.concatMap

instance CanFilter [a] a where
    filter = List.filter

instance CanFilterM [a] a where
    filterM = Monad.filterM

instance CanLength [a] Int where
    length = List.length

instance CanSingleton [a] a where
    singleton = return

instance CanNull [a] where
    null = List.null

instance CanPack [a] a where
    pack = id
    unpack = id
    subsequences = List.subsequences
    permutations = List.permutations

instance CanIntersperse [a] a where
    intersperse = List.intersperse

instance Monad m => CanMapM [i] (m [o]) m i o where
    mapM = Monad.mapM

instance CanMapM_ [a] a where
    mapM_ = Monad.mapM_

instance Eq k => CanLookup [(k, v)] k v where
    lookup = List.lookup

instance Eq k => CanInsertVal [(k, v)] k v where
    insertVal k v c = (k, v) : delete k c

instance Eq k => CanDeleteVal [(k, v)] k where
    deleteVal k = filter ((/= k) . fst)

instance Eq x => CanMember [x] x where
    member x = List.any (== x)

instance Eq a => CanStripPrefix [a] where
    stripPrefix = List.stripPrefix
    isPrefixOf = List.isPrefixOf

instance CanBreak [a] a where
    break = List.break
    span = List.span
    dropWhile = List.dropWhile
    takeWhile = List.takeWhile

instance CanAny [a] a where
    any = List.any
    all = List.all

instance CanSplitAt [c] Int where
    splitAt = List.splitAt

instance CanFold [a] a accum where
    fold = List.foldl'

instance (c ~ Char) => CanWords [c] where
    words = List.words
    unwords = List.unwords

instance (c ~ Char) => CanLines [c] where
    lines = List.lines

instance (c ~ Char) => CanUnlines [c] where
    unlines = List.unlines

instance Eq a => CanIsInfixOf [a] where
    isInfixOf = List.isInfixOf

instance CanReverse [a] where
    reverse = List.reverse

instance CanReplicate [i] i Int where
    replicate = List.replicate

instance CanReplicateM [a] a Int where
    replicateM = Monad.replicateM

instance CanFind [a] a where
    find = List.find

instance (Monoid m) => CanConcat [m] m where
    concat = Monoid.mconcat

instance CanPartition [a] a where
    partition = List.partition

instance CanNubBy [a] a where
    nubBy = List.nubBy

    nub =
        go Set.empty
      where
        go _ [] = []
        go set (x:xs)
            | x `Set.member` set = go set xs
            | otherwise = x : go (Set.insert x set) xs

instance (Eq a) => CanUnion [a] where
    union = List.union

instance (Eq a) => CanDifference [a] where
    difference = (List.\\)

instance (Eq a) => CanIntersection [a] where
    intersection = List.intersect

instance CanSortBy [a] a where
    sortBy = List.sortBy
instance Ord a => CanSort [a] a where
    sort = List.sort

instance CanCons [a] a where
    cons = (:)

instance CanUncons [a] a where
    uncons (head:tail) = Just (head, tail)
    uncons _ = Nothing

instance CanCompareLength [a] where
    compareLength [] 0 = EQ
    compareLength _ i | i <= 0 = GT
    compareLength [] _ = LT
    compareLength (_:t) i = compareLength t (i-1)

instance CanGroupBy [a] a where
    groupBy = List.groupBy

instance Eq a => CanGroup [a] a where
    group = List.group

instance CanRepeat [a] a where
    repeat = List.repeat

instance CanZipWith [a] a [b] b [c] c where
    zipWith = List.zipWith

instance CanZipWith3 [a] a [b] b [c] c [d] d where
    zipWith3 = List.zipWith3

instance CanZipWith4 [a] a [b] b [c] c [d] d [e] e where
    zipWith4 = List.zipWith4

instance CanZipWith5 [a] a [b] b [c] c [d] d [e] e [f] f where
    zipWith5 = List.zipWith5

instance CanZipWith6 [a] a [b] b [c] c [d] d [e] e [f] f [g] g where
    zipWith6 = List.zipWith6

instance CanZipWith7 [a] a [b] b [c] c [d] d [e] e [f] f [g] g [h] h where
    zipWith7 = List.zipWith7

instance CanZip ([] a) a ([] b) b [] where
    zip = List.zip

instance CanZip3 ([] a) a ([] b) b ([] c) c [] where
    zip3 = List.zip3

instance CanZip4 ([] a) a ([] b) b ([] c) c ([] d) d [] where
    zip4 = List.zip4

instance CanZip5 ([] a) a ([] b) b ([] c) c ([] d) d ([] e) e [] where
    zip5 = List.zip5

instance CanZip6 ([] a) a ([] b) b ([] c) c ([] d) d ([] e) e ([] f) f [] where
    zip6 = List.zip6

instance CanZip7 ([] a) a ([] b) b ([] c) c ([] d) d ([] e) e ([] f) f ([] g) g [] where
    zip7 = List.zip7

instance CanUnzip ([] a) a ([] b) b [] where
    unzip = List.unzip

instance CanUnzip3 ([] a) a ([] b) b ([] c) c [] where
    unzip3 = List.unzip3

instance CanUnzip4 ([] a) a ([] b) b ([] c) c ([] d) d [] where
    unzip4 = List.unzip4

instance CanUnzip5 ([] a) a ([] b) b ([] c) c ([] d) d ([] e) e [] where
    unzip5 = List.unzip5

instance CanUnzip6 ([] a) a ([] b) b ([] c) c ([] d) d ([] e) e ([] f) f [] where
    unzip6 = List.unzip6

instance CanUnzip7 ([] a) a ([] b) b ([] c) c ([] d) d ([] e) e ([] f) f ([] g) g [] where
    unzip7 = List.unzip7

instance CanEmpty [a] where
    empty = []
