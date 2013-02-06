{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.HashMap
    ( HashMap
    ) where

import CorePrelude
import ClassyPrelude.Classes
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HashMap


instance CanMapFunc (HashMap k v1) (HashMap k v2) v1 v2 where
    mapFunc = HashMap.map
    
instance Hashable k => CanFilterFunc (HashMap k v) (HashMap k v) (k, v) where
    filterFunc = HashMap.filterWithKey . curry
    
instance CanLength (HashMap k v) Int where
    length = HashMap.size
    
instance (Eq k, Hashable k, v' ~ v) => CanSingleton (v' -> HashMap k v) k where
    singleton = HashMap.singleton
    
instance CanNull (HashMap k v) where
    null = HashMap.null
    
instance (Eq k, Hashable k) => CanPack (HashMap k v) (k, v) where
    pack = HashMap.fromList
    unpack = HashMap.toList
    
instance (Eq k, Hashable k) => CanLookup (HashMap k v) k v where
    lookup = HashMap.lookup
    
instance (Eq k, Hashable k) => CanInsertVal (HashMap k v) k v where
    insertVal = HashMap.insert
    
instance (Eq k, Hashable k) => CanDeleteVal (HashMap k v) k where
    deleteVal = HashMap.delete
    
instance CanFind (HashMap k v) v where
    find = Foldable.find
    
instance (Monoid v) => CanConcat (HashMap k v) v where
    concat = Foldable.fold
    
instance Hashable k => CanPartition (HashMap k v) v where
    partition p m = (HashMap.filter p m, HashMap.filter (not . p) m)

instance (Hashable k, Eq k) => CanUnion (HashMap k a) where
    union = HashMap.union

instance (Hashable k, Eq k) => CanDifference (HashMap k a) where
    difference = HashMap.difference

instance (Hashable k, Eq k) => CanIntersection (HashMap k a) where
    intersection = HashMap.intersection
