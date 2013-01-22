{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.HashMap
    ( HashMap
    ) where

import qualified Prelude
import Prelude ((.), Char)
import ClassyPrelude.Classes
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Hashable (Hashable)

import Data.Monoid (Monoid)
import qualified Data.Monoid as Monoid
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable

type Map = HashMap

instance CanMapFunc (Map k v1) (Map k v2) v1 v2 where
    mapFunc = Map.map
instance Hashable k => CanFilterFunc (Map k v) (Map k v) (k, v) where
    filterFunc = Map.filterWithKey . Prelude.curry
instance CanLength (Map k v) Prelude.Int where
    length = Map.size
instance (Prelude.Eq k, Hashable k, v' ~ v) => CanSingleton (v' -> Map k v) k where
    singleton = Map.singleton
instance CanNull (Map k v) where
    null = Map.null
instance (Prelude.Eq k, Hashable k) => CanPack (Map k v) (k, v) where
    pack = Map.fromList
    unpack = Map.toList
instance (Prelude.Eq k, Hashable k) => CanLookup (Map k v) k v where
    lookup = Map.lookup
instance (Prelude.Eq k, Hashable k) => CanInsertVal (Map k v) k v where
    insertVal = Map.insert
instance (Prelude.Eq k, Hashable k) => CanDeleteVal (Map k v) k where
    deleteVal = Map.delete
instance CanFind (Map k v) v where
    find = Foldable.find
instance (Monoid v) => CanConcat (Map k v) v where
    concat = Foldable.fold