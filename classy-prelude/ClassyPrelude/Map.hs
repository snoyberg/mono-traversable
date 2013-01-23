{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.Map
    ( Map
    ) where

import qualified Prelude
import Prelude ((.), Char)
import ClassyPrelude.Classes
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Monoid (Monoid)
import qualified Data.Monoid as Monoid
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable

instance CanMapFunc (Map k v1) (Map k v2) v1 v2 where
    mapFunc = Map.map
instance Prelude.Ord k => CanFilterFunc (Map k v) (Map k v) (k, v) where
    filterFunc = Map.filterWithKey . Prelude.curry
instance CanLength (Map k v) Prelude.Int where
    length = Map.size
instance (v' ~ v) => CanSingleton (v' -> Map k v) k where
    singleton = Map.singleton
instance CanNull (Map k v) where
    null = Map.null
instance Prelude.Ord k => CanPack (Map k v) (k, v) where
    pack = Map.fromList
    unpack = Map.toList
instance Prelude.Ord k => CanLookup (Map k v) k v where
    lookup = Map.lookup
instance Prelude.Ord k => CanInsertVal (Map k v) k v where
    insertVal = Map.insert
instance Prelude.Ord k => CanDeleteVal (Map k v) k where
    deleteVal = Map.delete
instance CanFind (Map k v) v where
    find = Foldable.find
instance (Monoid v) => CanConcat (Map k v) v where
    concat = Foldable.fold
instance Prelude.Ord k => CanPartition (Map k v) v where
    partition = Map.partition
