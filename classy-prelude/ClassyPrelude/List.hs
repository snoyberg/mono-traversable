{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.List () where

import ClassyPrelude.Classes
import Prelude ((.), ($), otherwise, Maybe(..), Monad, Ord, Eq, Int, Bool, Char, Bool(..))
import Data.Monoid (Monoid)
import qualified Prelude
import qualified Data.List as List
import qualified Control.Monad as Monad
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set


instance CanMapFunc [a] [b] a b where
    mapFunc = Prelude.map

instance CanConcatMapFunc [a] [b] a [b] where
    concatMapFunc = Prelude.concatMap

instance CanFilterFunc [a] [a] a where
    filterFunc = Prelude.filter

instance CanFilterMFunc [a] a where
    filterMFunc = Monad.filterM

instance CanLength [a] Int where
    length = Prelude.length

instance CanSingleton [a] a where
    singleton = Prelude.return

instance CanNull [a] where
    null = Prelude.null

instance CanPack [a] a where
    pack = Prelude.id
    unpack = Prelude.id
    subsequences = List.subsequences
    permutations = List.permutations

instance CanIntersperse [a] a where
    intersperse = List.intersperse

instance Monad m => CanMapMFunc [i] (m [o]) m i o where
    mapMFunc = Prelude.mapM

instance CanMapM_Func [a] a where
    mapM_Func = Prelude.mapM_

instance Eq k => CanLookup [(k, v)] k v where
    lookup = Prelude.lookup

instance Eq k => CanInsertVal [(k, v)] k v where
    insertVal k v c = (k, v) : delete k c

instance Eq k => CanDeleteVal [(k, v)] k where
    deleteVal k = filter ((Prelude./= k) . Prelude.fst)

instance Eq x => CanMember [x] x where
    member x = Prelude.any (Prelude.== x)

instance Eq a => CanStripPrefix [a] where
    stripPrefix = List.stripPrefix
    isPrefixOf = List.isPrefixOf

instance CanBreak [a] a where
    break = Prelude.break
    span = Prelude.span
    dropWhile = Prelude.dropWhile
    takeWhile = Prelude.takeWhile

instance CanAny [a] a where
    any = Prelude.any
    all = Prelude.all

instance CanSplitAt [c] Int where
    splitAt = Prelude.splitAt

instance CanFoldFunc [a] a accum where
    foldFunc = List.foldl'

instance (c ~ Char) => CanWords [c] where
    words = Prelude.words
    unwords = Prelude.unwords

instance (c ~ Char) => CanLinesFunc [c] where
    linesFunc = Prelude.lines

instance (c ~ Char) => CanUnlines [c] where
    unlines = Prelude.unlines

instance Eq a => CanIsInfixOf [a] where
    isInfixOf = List.isInfixOf

instance CanReverse [a] where
    reverse = Prelude.reverse

instance CanReplicate [i] i Int where
    replicate = Prelude.replicate

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
