{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Containers where

import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import qualified Data.Set as Set
import qualified Data.HashSet as HashSet
import Data.Monoid (Monoid)
import Data.MonoTraversable (MonoFoldable, MonoTraversable, Element)
import qualified Data.IntMap as IntMap
import Data.Function (on)
import qualified Data.List as List

class (Monoid c, MonoFoldable c) => Container c where
    type ContainerKey c
    member :: ContainerKey c -> c -> Bool
    notMember ::  ContainerKey c -> c -> Bool
    union :: c -> c -> c
    difference :: c -> c -> c
    intersection :: c -> c -> c
instance Ord k => Container (Map.Map k v) where
    type ContainerKey (Map.Map k v) = k
    member = Map.member
    notMember = Map.notMember
    union = Map.union
    difference = Map.difference
    intersection = Map.intersection
instance (Eq k, Hashable k) => Container (HashMap.HashMap k v) where
    type ContainerKey (HashMap.HashMap k v) = k
    member = HashMap.member
    notMember k = not . HashMap.member k
    union = HashMap.union
    difference = HashMap.difference
    intersection = HashMap.intersection
instance Container (IntMap.IntMap v) where
    type ContainerKey (IntMap.IntMap v) = Int
    member = IntMap.member
    notMember = IntMap.notMember
    union = IntMap.union
    difference = IntMap.difference
    intersection = IntMap.intersection
instance Ord e => Container (Set.Set e) where
    type ContainerKey (Set.Set e) = e
    member = Set.member
    notMember = Set.notMember
    union = Set.union
    difference = Set.difference
    intersection = Set.intersection
instance (Eq e, Hashable e) => Container (HashSet.HashSet e) where
    type ContainerKey (HashSet.HashSet e) = e
    member = HashSet.member
    notMember e = not . HashSet.member e
    union = HashSet.union
    difference = HashSet.difference
    intersection = HashSet.intersection
instance Ord k => Container [(k, v)] where
    type ContainerKey [(k, v)] = k
    member k = List.any ((== k) . fst)
    notMember k = not . member k
    union = List.unionBy ((==) `on` fst)
    x `difference` y = Map.toList (Map.fromList x `Map.difference` Map.fromList y)
    intersection = List.intersectBy ((==) `on` fst)

class (MonoTraversable m, Container m) => IsMap m where
    -- | Using just @Element@ can lead to very confusing error messages.
    type MapValue m
    lookup :: ContainerKey m -> m -> Maybe (MapValue m)
    insertMap :: ContainerKey m -> MapValue m -> m -> m
    deleteMap :: ContainerKey m -> m -> m
    singletonMap :: ContainerKey m -> MapValue m -> m
    mapFromList :: [(ContainerKey m, MapValue m)] -> m
    mapToList :: m -> [(ContainerKey m, MapValue m)]
instance Ord k => IsMap (Map.Map k v) where
    type MapValue (Map.Map k v) = v
    lookup = Map.lookup
    insertMap = Map.insert
    deleteMap = Map.delete
    singletonMap = Map.singleton
    mapFromList = Map.fromList
    mapToList = Map.toList
instance (Eq k, Hashable k) => IsMap (HashMap.HashMap k v) where
    type MapValue (HashMap.HashMap k v) = v
    lookup = HashMap.lookup
    insertMap = HashMap.insert
    deleteMap = HashMap.delete
    singletonMap = HashMap.singleton
    mapFromList = HashMap.fromList
    mapToList = HashMap.toList
instance IsMap (IntMap.IntMap v) where
    type MapValue (IntMap.IntMap v) = v
    lookup = IntMap.lookup
    insertMap = IntMap.insert
    deleteMap = IntMap.delete
    singletonMap = IntMap.singleton
    mapFromList = IntMap.fromList
    mapToList = IntMap.toList
instance Ord k => IsMap [(k, v)] where
    type MapValue [(k, v)] = v
    lookup = List.lookup
    insertMap k v = ((k, v):) . deleteMap k
    deleteMap k = List.filter ((/= k) . fst)
    singletonMap k v = [(k, v)]
    mapFromList = id
    mapToList = id

class (Container s, Element s ~ ContainerKey s) => IsSet s where
    insertSet :: Element s -> s -> s
    deleteSet :: Element s -> s -> s
    singletonSet :: Element s -> s
    setFromList :: [Element s] -> s
    setToList :: s -> [Element s]
instance Ord e => IsSet (Set.Set e) where
    insertSet = Set.insert
    deleteSet = Set.delete
    singletonSet = Set.singleton
    setFromList = Set.fromList
    setToList = Set.toList
instance (Eq e, Hashable e) => IsSet (HashSet.HashSet e) where
    insertSet = HashSet.insert
    deleteSet = HashSet.delete
    singletonSet = HashSet.singleton
    setFromList = HashSet.fromList
    setToList = HashSet.toList
