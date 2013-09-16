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
import Data.MonoTraversable (MonoFoldable, MonoTraversable, ElementOf)
import qualified Data.IntMap as IntMap
import Data.Function (on)
import qualified Data.List as List
import qualified Data.IntSet as IntSet

class (Monoid container, MonoFoldable container) => SetContainer container where
    type KeyOf container
    member       :: KeyOf container -> container -> Bool
    notMember    :: KeyOf container -> container -> Bool
    union        :: container -> container -> container
    difference   :: container -> container -> container
    intersection :: container -> container -> container

instance Ord key => SetContainer (Map.Map key value) where
    type KeyOf (Map.Map key value) = key
    member = Map.member
    notMember = Map.notMember
    union = Map.union
    difference = Map.difference
    intersection = Map.intersection

instance (Eq key, Hashable key) => SetContainer (HashMap.HashMap key value) where
    type KeyOf (HashMap.HashMap key value) = key
    member = HashMap.member
    notMember k = not . HashMap.member k
    union = HashMap.union
    difference = HashMap.difference
    intersection = HashMap.intersection

instance SetContainer (IntMap.IntMap value) where
    type KeyOf (IntMap.IntMap value) = Int
    member = IntMap.member
    notMember = IntMap.notMember
    union = IntMap.union
    difference = IntMap.difference
    intersection = IntMap.intersection

instance Ord element => SetContainer (Set.Set element) where
    type KeyOf (Set.Set element) = element
    member = Set.member
    notMember = Set.notMember
    union = Set.union
    difference = Set.difference
    intersection = Set.intersection

instance (Eq element, Hashable element) => SetContainer (HashSet.HashSet element) where
    type KeyOf (HashSet.HashSet element) = element
    member = HashSet.member
    notMember e = not . HashSet.member e
    union = HashSet.union
    difference = HashSet.difference
    intersection = HashSet.intersection

instance SetContainer IntSet.IntSet where
    type KeyOf IntSet.IntSet = Int
    member = IntSet.member
    notMember = IntSet.notMember
    union = IntSet.union
    difference = IntSet.difference
    intersection = IntSet.intersection

instance Ord key => SetContainer [(key, value)] where
    type KeyOf [(key, value)] = key
    member k = List.any ((== k) . fst)
    notMember k = not . member k
    union = List.unionBy ((==) `on` fst)
    x `difference` y = Map.toList (Map.fromList x `Map.difference` Map.fromList y)
    intersection = List.intersectBy ((==) `on` fst)

class (MonoTraversable map, SetContainer map) => IsMap map where
    -- | Using just @ElementOf@ can lead to very confusing error messages.
    type ValueOf map
    lookup       :: KeyOf map -> map -> Maybe (ValueOf map)
    insertMap    :: KeyOf map -> ValueOf map -> map -> map
    deleteMap    :: KeyOf map -> map -> map
    singletonMap :: KeyOf map -> ValueOf map -> map
    mapFromList  :: [(KeyOf map, ValueOf map)] -> map
    mapToList    :: map -> [(KeyOf map, ValueOf map)]

instance Ord key => IsMap (Map.Map key value) where
    type ValueOf (Map.Map key value) = value
    lookup = Map.lookup
    insertMap = Map.insert
    deleteMap = Map.delete
    singletonMap = Map.singleton
    mapFromList = Map.fromList
    mapToList = Map.toList

instance (Eq key, Hashable key) => IsMap (HashMap.HashMap key value) where
    type ValueOf (HashMap.HashMap key value) = value
    lookup = HashMap.lookup
    insertMap = HashMap.insert
    deleteMap = HashMap.delete
    singletonMap = HashMap.singleton
    mapFromList = HashMap.fromList
    mapToList = HashMap.toList

instance IsMap (IntMap.IntMap value) where
    type ValueOf (IntMap.IntMap value) = value
    lookup = IntMap.lookup
    insertMap = IntMap.insert
    deleteMap = IntMap.delete
    singletonMap = IntMap.singleton
    mapFromList = IntMap.fromList
    mapToList = IntMap.toList

instance Ord key => IsMap [(key, value)] where
    type ValueOf [(key, value)] = value
    lookup = List.lookup
    insertMap k v = ((k, v):) . deleteMap k
    deleteMap k = List.filter ((/= k) . fst)
    singletonMap k v = [(k, v)]
    mapFromList = id
    mapToList = id

class (SetContainer set, ElementOf set ~ KeyOf set) => IsSet set where
    insertSet :: ElementOf set -> set -> set
    deleteSet :: ElementOf set -> set -> set
    singletonSet :: ElementOf set -> set
    setFromList :: [ElementOf set] -> set
    setToList :: set -> [ElementOf set]

instance Ord element => IsSet (Set.Set element) where
    insertSet = Set.insert
    deleteSet = Set.delete
    singletonSet = Set.singleton
    setFromList = Set.fromList
    setToList = Set.toList

instance (Eq element, Hashable element) => IsSet (HashSet.HashSet element) where
    insertSet = HashSet.insert
    deleteSet = HashSet.delete
    singletonSet = HashSet.singleton
    setFromList = HashSet.fromList
    setToList = HashSet.toList

instance IsSet IntSet.IntSet where
    insertSet = IntSet.insert
    deleteSet = IntSet.delete
    singletonSet = IntSet.singleton
    setFromList = IntSet.fromList
    setToList = IntSet.toList
