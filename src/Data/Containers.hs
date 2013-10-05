{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
-- | Warning: This module should be considered highly experimental.
module Data.Containers where

import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import qualified Data.Set as Set
import qualified Data.HashSet as HashSet
import Data.Monoid (Monoid)
import Data.MonoTraversable (MonoFunctor(..), MonoFoldable, MonoTraversable, Element)
import qualified Data.IntMap as IntMap
import Data.Function (on)
import qualified Data.List as List
import qualified Data.IntSet as IntSet

import qualified Data.Text.Lazy as LText
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.ByteString as ByteString
import Control.Arrow ((&&&), (***))

class (Monoid set, MonoFoldable set) => SetContainer set where
    type ContainerKey set
    member :: ContainerKey set -> set -> Bool
    notMember ::  ContainerKey set -> set -> Bool
    union :: set -> set -> set
    difference :: set -> set -> set
    intersection :: set -> set -> set
instance Ord k => SetContainer (Map.Map k v) where
    type ContainerKey (Map.Map k v) = k
    member = Map.member
    notMember = Map.notMember
    union = Map.union
    difference = Map.difference
    intersection = Map.intersection

instance (Eq key, Hashable key) => SetContainer (HashMap.HashMap key value) where
    type ContainerKey (HashMap.HashMap key value) = key
    member = HashMap.member
    notMember k = not . HashMap.member k
    union = HashMap.union
    difference = HashMap.difference
    intersection = HashMap.intersection

instance SetContainer (IntMap.IntMap value) where
    type ContainerKey (IntMap.IntMap value) = Int
    member = IntMap.member
    notMember = IntMap.notMember
    union = IntMap.union
    difference = IntMap.difference
    intersection = IntMap.intersection

instance Ord element => SetContainer (Set.Set element) where
    type ContainerKey (Set.Set element) = element
    member = Set.member
    notMember = Set.notMember
    union = Set.union
    difference = Set.difference
    intersection = Set.intersection

instance (Eq element, Hashable element) => SetContainer (HashSet.HashSet element) where
    type ContainerKey (HashSet.HashSet element) = element
    member = HashSet.member
    notMember e = not . HashSet.member e
    union = HashSet.union
    difference = HashSet.difference
    intersection = HashSet.intersection

instance SetContainer IntSet.IntSet where
    type ContainerKey IntSet.IntSet = Int
    member = IntSet.member
    notMember = IntSet.notMember
    union = IntSet.union
    difference = IntSet.difference
    intersection = IntSet.intersection

instance Ord key => SetContainer [(key, value)] where
    type ContainerKey [(key, value)] = key
    member k = List.any ((== k) . fst)
    notMember k = not . member k
    union = List.unionBy ((==) `on` fst)
    x `difference` y = Map.toList (Map.fromList x `Map.difference` Map.fromList y)
    intersection = List.intersectBy ((==) `on` fst)

class (MonoTraversable map, SetContainer map) => IsMap map where
    -- | Using just @Element@ can lead to very confusing error messages.
    type MapValue map
    lookup       :: ContainerKey map -> map -> Maybe (MapValue map)
    insertMap    :: ContainerKey map -> MapValue map -> map -> map
    deleteMap    :: ContainerKey map -> map -> map
    singletonMap :: ContainerKey map -> MapValue map -> map
    mapFromList  :: [(ContainerKey map, MapValue map)] -> map
    mapToList    :: map -> [(ContainerKey map, MapValue map)]

instance Ord key => IsMap (Map.Map key value) where
    type MapValue (Map.Map key value) = value
    lookup = Map.lookup
    insertMap = Map.insert
    deleteMap = Map.delete
    singletonMap = Map.singleton
    mapFromList = Map.fromList
    mapToList = Map.toList

instance (Eq key, Hashable key) => IsMap (HashMap.HashMap key value) where
    type MapValue (HashMap.HashMap key value) = value
    lookup = HashMap.lookup
    insertMap = HashMap.insert
    deleteMap = HashMap.delete
    singletonMap = HashMap.singleton
    mapFromList = HashMap.fromList
    mapToList = HashMap.toList

instance IsMap (IntMap.IntMap value) where
    type MapValue (IntMap.IntMap value) = value
    lookup = IntMap.lookup
    insertMap = IntMap.insert
    deleteMap = IntMap.delete
    singletonMap = IntMap.singleton
    mapFromList = IntMap.fromList
    mapToList = IntMap.toList

instance Ord key => IsMap [(key, value)] where
    type MapValue [(key, value)] = value
    lookup = List.lookup
    insertMap k v = ((k, v):) . deleteMap k
    deleteMap k = List.filter ((/= k) . fst)
    singletonMap k v = [(k, v)]
    mapFromList = id
    mapToList = id

class (SetContainer set, Element set ~ ContainerKey set) => IsSet set where
    insertSet :: Element set -> set -> set
    deleteSet :: Element set -> set -> set
    singletonSet :: Element set -> set
    setFromList :: [Element set] -> set
    setToList :: set -> [Element set]

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



class (MonoFunctor mfIn, MonoFunctor mfOut, Functor f)
     => MonoZip mfIn mfOut f -- | mfIn -> f
  where
    ozipWith :: (Element mfIn -> Element mfIn -> Element mfOut) -> mfIn -> mfIn -> mfOut

    ozip :: mfIn -> mfIn -> f (Element mfIn, Element mfIn)
    -- ozip = ozipWith (,)

    ounzip :: f (Element mfIn, Element mfIn) -> (mfIn, mfIn)
    -- ounzip = omap fst &&& omap snd

{-
    ozap :: f (Element mfIn -> Element mfOut) -> mfIn -> mfOut
    ozap = ozipWith id
-}

instance MonoZip ByteString.ByteString [a] [] where
    ozip = ByteString.zip
    ounzip = ByteString.unzip
    ozipWith = ByteString.zipWith
instance MonoZip LByteString.ByteString [a] [] where
    ozip = LByteString.zip
    ounzip = LByteString.unzip
    ozipWith = LByteString.zipWith
instance MonoZip Text.Text Text.Text [] where
    ozip = Text.zip
    ounzip = (Text.pack *** Text.pack) . List.unzip
    ozipWith = Text.zipWith
instance MonoZip LText.Text LText.Text [] where
    ozip = LText.zip
    ounzip = (LText.pack *** LText.pack) . List.unzip
    ozipWith = LText.zipWith

