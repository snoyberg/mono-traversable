{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.List () where

import qualified Prelude
import Prelude ((.))
import ClassyPrelude.Classes
import qualified Data.List
import qualified Control.Monad

instance CanMapFunc [a] [b] a b where
    mapFunc = Prelude.map
instance CanConcatMapFunc [a] [b] a [b] where
    concatMapFunc = Prelude.concatMap
instance CanFilterFunc [a] [a] a where
    filterFunc = Prelude.filter
instance CanFilterMFunc [a] a where
    filterMFunc = Control.Monad.filterM
instance CanLength [a] Prelude.Int where
    length = Prelude.length
instance CanSingleton [a] a where
    singleton = Prelude.return
instance CanNull [a] where
    null = Prelude.null
instance CanPack [a] a where
    pack = Prelude.id
    unpack = Prelude.id
instance Prelude.Monad m => CanMapMFunc [i] (m [o]) m i o where
    mapMFunc = Prelude.mapM
instance CanMapM_Func [a] a where
    mapM_Func = Prelude.mapM_
instance Prelude.Eq k => CanLookup [(k, v)] k v where
    lookup = Prelude.lookup
instance Prelude.Eq k => CanInsertVal [(k, v)] k v where
    insertVal k v c = (k, v) : delete k c
instance Prelude.Eq k => CanDeleteVal [(k, v)] k where
    deleteVal k = filter ((Prelude./= k) . Prelude.fst)
instance Prelude.Eq x => CanMember [x] x where
    member x = Prelude.any (Prelude.== x)
instance Prelude.Eq a => CanStripPrefix [a] where
    stripPrefix = Data.List.stripPrefix
    isPrefixOf = Data.List.isPrefixOf
instance CanBreak [a] a where
    break = Prelude.break
    span = Prelude.span
    dropWhile = Prelude.dropWhile
    takeWhile = Prelude.takeWhile
instance CanAny [a] a where
    any = Prelude.any
    all = Prelude.all
instance CanSplitAt [c] Prelude.Int where
    splitAt = Prelude.splitAt
instance CanFoldFunc [a] a accum where
    foldFunc = Data.List.foldl'

instance (c ~ Prelude.Char) => CanWords [c] where
    words = Prelude.words
    unwords = Prelude.unwords

instance (c ~ Prelude.Char) => CanLinesFunc [c] where
    linesFunc = Prelude.lines

instance (c ~ Prelude.Char) => CanUnlines [c] where
    unlines = Prelude.unlines

instance Prelude.Eq a => CanIsInfixOf [a] where
    isInfixOf = Data.List.isInfixOf

instance CanReverse [a] where
    reverse = Prelude.reverse

instance CanReplicate [i] i Prelude.Int where
    replicate = Prelude.replicate

instance CanReplicateM [a] a Prelude.Int where
    replicateM = Control.Monad.replicateM

instance CanFind [a] a where
    find = Data.List.find
    