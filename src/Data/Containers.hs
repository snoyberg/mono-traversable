{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Containers where

import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import qualified Data.Set as Set
import qualified Data.HashSet as HashSet

class Container c k | c -> k where
    member :: k -> c -> Bool
    notMember :: k -> c -> Bool
    union :: c -> c -> c
    difference :: c -> c -> c
    intersection :: c -> c -> c
instance Ord k => Container (Map.Map k v) k where
    member = Map.member
    notMember = Map.notMember
    union = Map.union
    difference = Map.difference
    intersection = Map.intersection
instance (Eq k, Hashable k) => Container (HashMap.HashMap k v) k where
    member = HashMap.member
    notMember k = not . HashMap.member k
    union = HashMap.union
    difference = HashMap.difference
    intersection = HashMap.intersection
instance Ord e => Container (Set.Set e) e where
    member = Set.member
    notMember = Set.notMember
    union = Set.union
    difference = Set.difference
    intersection = Set.intersection
instance (Eq e, Hashable e) => Container (HashSet.HashSet e) e where
    member = HashSet.member
    notMember e = not . HashSet.member e
    union = HashSet.union
    difference = HashSet.difference
    intersection = HashSet.intersection

class Container m k => IsMap m k v | m -> k v where
    lookup :: k -> m -> Maybe v
    insertMap :: k -> v -> m -> m
    deleteMap :: k -> m -> m
instance Ord k => IsMap (Map.Map k v) k v where
    lookup = Map.lookup
    insertMap = Map.insert
    deleteMap = Map.delete
instance (Eq k, Hashable k) => IsMap (HashMap.HashMap k v) k v where
    lookup = HashMap.lookup
    insertMap = HashMap.insert
    deleteMap = HashMap.delete

class Container s e => IsSet s e | s -> e where
    insertSet :: e -> s -> s
    deleteSet :: e -> s -> s
instance Ord e => IsSet (Set.Set e) e where
    insertSet = Set.insert
    deleteSet = Set.delete
instance (Eq e, Hashable e) => IsSet (HashSet.HashSet e) e where
    insertSet = HashSet.insert
    deleteSet = HashSet.delete