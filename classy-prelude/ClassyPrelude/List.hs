{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.List () where

import qualified Prelude
import Prelude ((.))
import ClassyPrelude.Classes
import qualified Data.List

instance (a ~ a', b ~ b') => CanMap [a] [b] a' b' where
    map = Prelude.map
instance (a ~ a', b ~ b') => CanConcatMap [a] [b] a' [b'] where
    concatMap = Prelude.concatMap
instance CanFilter [a] a where
    filter = Prelude.filter
instance CanLength [a] Prelude.Int where
    length = Prelude.length
instance CanSingleton [a] a where
    singleton = Prelude.return
instance CanNull [a] where
    null = Prelude.null
instance CanPack [a] a where
    pack = Prelude.id
    unpack = Prelude.id
instance (a ~ a', b ~ [b']) => CanMapM [a] b a' b' where
    mapM = Prelude.mapM
instance a ~ a' => CanMapM_ [a] a' where
    mapM_ = Prelude.mapM_
instance Prelude.Eq k => CanLookup [(k, v)] k v where
    lookup = Prelude.lookup
instance CanEmpty [a] where
    empty = []
instance Prelude.Eq k => CanInsertVal [(k, v)] k v where
    insertVal k v c = (k, v) : delete k c
instance Prelude.Eq k => CanDelete [(k, v)] k where
    delete k = filter ((Prelude./= k) . Prelude.fst)
instance Prelude.Eq x => CanMember [x] x where
    member x = Prelude.any (Prelude.== x)
instance Prelude.Eq a => CanStripPrefix [a] where
    stripPrefix = Data.List.stripPrefix
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
instance CanFold [a] a accum where
    fold = Data.List.foldl'
