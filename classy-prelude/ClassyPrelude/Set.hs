{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.Set
    ( Set
    ) where

import CorePrelude
import ClassyPrelude.Classes
import qualified Control.Monad as Monad
import qualified Data.Foldable as Foldable
import qualified Data.Set as Set


instance (Ord a, Ord b) => CanMap (Set a) (Set b) a b where
    map = Set.map

instance CanFilter (Set a) a where
    filter = Set.filter

instance CanLength (Set x) Int where
    length = Set.size

instance CanSingleton (Set x) x where
    singleton = Set.singleton

instance CanNull (Set x) where
    null = Set.null

instance Ord x => CanPack (Set x) x where
    pack = Set.fromList
    unpack = Set.toList

instance (Ord x, Set x ~ s, x ~ x') => CanInsert (x' -> s -> Set x) where
    insert = Set.insert

instance Ord x => CanDeleteVal (Set x) x where
    deleteVal = Set.delete

instance Ord x => CanMember (Set x) x where
    member = Set.member

instance CanFold (Set a) a accum where
    fold = Set.foldl'

instance Ord a => CanMapM_ (Set a) a where
    mapM_ f = Monad.mapM_ f . unpack

instance CanFind (Set a) a where
    find = Foldable.find

instance (Monoid m) => CanConcat (Set m) m where
    concat = Foldable.fold

instance (Ord a) => CanPartition (Set a) a where
    partition = Set.partition

instance (Ord a) => CanUnion (Set a) where
    union = Set.union

instance (Ord a) => CanDifference (Set a) where
    difference = Set.difference

instance (Ord a) => CanIntersection (Set a) where
    intersection = Set.intersection
