{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
-- | Warning: This module should be considered highly experimental.
module Data.Containers where

import Prelude hiding (lookup)
import Data.Maybe (fromMaybe)
#if MIN_VERSION_containers(0, 5, 0)
import qualified Data.Map.Strict as Map
#else
import qualified Data.Map as Map
#endif
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import qualified Data.Set as Set
import qualified Data.HashSet as HashSet
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup)
import Data.MonoTraversable (MonoFunctor(..), MonoFoldable, MonoTraversable, Element)
import qualified Data.IntMap as IntMap
import Data.Function (on)
import qualified Data.List as List
import qualified Data.IntSet as IntSet

import qualified Data.Text.Lazy as LText
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.ByteString as ByteString
import Control.Arrow ((***))

class (Monoid set, Semigroup set, MonoFoldable set, Eq (ContainerKey set)) => SetContainer set where
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

instance Eq key => SetContainer [(key, value)] where
    type ContainerKey [(key, value)] = key
    member k = List.any ((== k) . fst)
    notMember k = not . member k
    union = List.unionBy ((==) `on` fst)
    x `difference` y =
        loop x
      where
        loop [] = []
        loop ((k, v):rest) =
            case lookup k y of
                Nothing -> (k, v) : loop rest
                Just _ -> loop rest
    intersection = List.intersectBy ((==) `on` fst)

-- | A guaranteed-polymorphic @Map@, which allows for more polymorphic versions
-- of functions.
class PolyMap map where
    differenceMap :: map value1 -> map value2 -> map value1
    {-
    differenceWithMap :: (value1 -> value2 -> Maybe value1)
                      -> map value1 -> map value2 -> map value1
    -}
    intersectionMap :: map value1 -> map value2 -> map value1
    intersectionWithMap :: (value1 -> value2 -> value3)
                        -> map value1 -> map value2 -> map value3

instance Ord key => PolyMap (Map.Map key) where
    differenceMap = Map.difference
    --differenceWithMap = Map.differenceWith
    intersectionMap = Map.intersection
    intersectionWithMap = Map.intersectionWith

instance (Eq key, Hashable key) => PolyMap (HashMap.HashMap key) where
    differenceMap = HashMap.difference
    --differenceWithMap = HashMap.differenceWith
    intersectionMap = HashMap.intersection
    intersectionWithMap = HashMap.intersectionWith

instance PolyMap IntMap.IntMap where
    differenceMap = IntMap.difference
    --differenceWithMap = IntMap.differenceWith
    intersectionMap = IntMap.intersection
    intersectionWithMap = IntMap.intersectionWith

class (MonoTraversable map, SetContainer map) => IsMap map where
    -- | In some cases, @MapValue@ and @Element@ will be different, e.g., the
    -- @IsMap@ instance of associated lists.
    type MapValue map
    lookup       :: ContainerKey map -> map -> Maybe (MapValue map)
    insertMap    :: ContainerKey map -> MapValue map -> map -> map
    deleteMap    :: ContainerKey map -> map -> map
    singletonMap :: ContainerKey map -> MapValue map -> map
    mapFromList  :: [(ContainerKey map, MapValue map)] -> map
    mapToList    :: map -> [(ContainerKey map, MapValue map)]

    findWithDefault :: MapValue map -> ContainerKey map -> map -> MapValue map
    findWithDefault def key = fromMaybe def . lookup key

    insertWith :: (MapValue map -> MapValue map -> MapValue map)
               -> ContainerKey map
               -> MapValue map
               -> map
               -> map
    insertWith f k v m =
        v' `seq` insertMap k v' m
      where
        v' =
            case lookup k m of
                Nothing -> v
                Just vold -> f v vold

    insertWithKey
        :: (ContainerKey map -> MapValue map -> MapValue map -> MapValue map)
        -> ContainerKey map
        -> MapValue map
        -> map
        -> map
    insertWithKey f k v m =
        v' `seq` insertMap k v' m
      where
        v' =
            case lookup k m of
                Nothing -> v
                Just vold -> f k v vold

    insertLookupWithKey
        :: (ContainerKey map -> MapValue map -> MapValue map -> MapValue map)
        -> ContainerKey map
        -> MapValue map
        -> map
        -> (Maybe (MapValue map), map)
    insertLookupWithKey f k v m =
        v' `seq` (mold, insertMap k v' m)
      where
        (mold, v') =
            case lookup k m of
                Nothing -> (Nothing, v)
                Just vold -> (Just vold, f k v vold)

    adjustMap
        :: (MapValue map -> MapValue map)
        -> ContainerKey map
        -> map
        -> map
    adjustMap f k m =
        case lookup k m of
            Nothing -> m
            Just v ->
                let v' = f v
                 in v' `seq` insertMap k v' m

    adjustWithKey
        :: (ContainerKey map -> MapValue map -> MapValue map)
        -> ContainerKey map
        -> map
        -> map
    adjustWithKey f k m =
        case lookup k m of
            Nothing -> m
            Just v ->
                let v' = f k v
                 in v' `seq` insertMap k v' m

    updateMap
        :: (MapValue map -> Maybe (MapValue map))
        -> ContainerKey map
        -> map
        -> map
    updateMap f k m =
        case lookup k m of
            Nothing -> m
            Just v ->
                case f v of
                    Nothing -> deleteMap k m
                    Just v' -> v' `seq` insertMap k v' m

    updateWithKey
        :: (ContainerKey map -> MapValue map -> Maybe (MapValue map))
        -> ContainerKey map
        -> map
        -> map
    updateWithKey f k m =
        case lookup k m of
            Nothing -> m
            Just v ->
                case f k v of
                    Nothing -> deleteMap k m
                    Just v' -> v' `seq` insertMap k v' m

    updateLookupWithKey
        :: (ContainerKey map -> MapValue map -> Maybe (MapValue map))
        -> ContainerKey map
        -> map
        -> (Maybe (MapValue map), map)
    updateLookupWithKey f k m =
        case lookup k m of
            Nothing -> (Nothing, m)
            Just v ->
                case f k v of
                    Nothing -> (Just v, deleteMap k m)
                    Just v' -> v' `seq` (Just v', insertMap k v' m)

    alterMap
        :: (Maybe (MapValue map) -> Maybe (MapValue map))
        -> ContainerKey map
        -> map
        -> map
    alterMap f k m =
        case f mold of
            Nothing ->
                case mold of
                    Nothing -> m
                    Just _ -> deleteMap k m
            Just v -> insertMap k v m
      where
        mold = lookup k m

    unionWith
        :: (MapValue map -> MapValue map -> MapValue map)
        -> map
        -> map
        -> map
    unionWith f x y =
        mapFromList $ loop $ mapToList x ++ mapToList y
      where
        loop [] = []
        loop ((k, v):rest) =
            case List.lookup k rest of
                Nothing -> (k, v) : loop rest
                Just v' -> (k, f v v') : loop (deleteMap k rest)

    unionWithKey
        :: (ContainerKey map -> MapValue map -> MapValue map -> MapValue map)
        -> map
        -> map
        -> map
    unionWithKey f x y =
        mapFromList $ loop $ mapToList x ++ mapToList y
      where
        loop [] = []
        loop ((k, v):rest) =
            case List.lookup k rest of
                Nothing -> (k, v) : loop rest
                Just v' -> (k, f k v v') : loop (deleteMap k rest)

    unionsWith
        :: (MapValue map -> MapValue map -> MapValue map)
        -> [map]
        -> map
    unionsWith _ [] = mempty
    unionsWith _ [x] = x
    unionsWith f (x:y:z) = unionsWith f (unionWith f x y:z)

    mapWithKey
        :: (ContainerKey map -> MapValue map -> MapValue map)
        -> map
        -> map
    mapWithKey f =
        mapFromList . map go . mapToList
      where
        go (k, v) = (k, f k v)

    mapKeysWith
        :: (MapValue map -> MapValue map -> MapValue map)
        -> (ContainerKey map -> ContainerKey map)
        -> map
        -> map
    mapKeysWith g f =
        mapFromList . unionsWith g . map go . mapToList
      where
        go (k, v) = [(f k, v)]

instance Ord key => IsMap (Map.Map key value) where
    type MapValue (Map.Map key value) = value
    lookup = Map.lookup
    insertMap = Map.insert
    deleteMap = Map.delete
    singletonMap = Map.singleton
    mapFromList = Map.fromList
    mapToList = Map.toList

    findWithDefault = Map.findWithDefault
    insertWith = Map.insertWith
    insertWithKey = Map.insertWithKey
    insertLookupWithKey = Map.insertLookupWithKey
    adjustMap = Map.adjust
    adjustWithKey = Map.adjustWithKey
    updateMap = Map.update
    updateWithKey = Map.updateWithKey
    updateLookupWithKey = Map.updateLookupWithKey
    alterMap = Map.alter
    unionWith = Map.unionWith
    unionWithKey = Map.unionWithKey
    unionsWith = Map.unionsWith
    mapWithKey = Map.mapWithKey
    mapKeysWith = Map.mapKeysWith

instance (Eq key, Hashable key) => IsMap (HashMap.HashMap key value) where
    type MapValue (HashMap.HashMap key value) = value
    lookup = HashMap.lookup
    insertMap = HashMap.insert
    deleteMap = HashMap.delete
    singletonMap = HashMap.singleton
    mapFromList = HashMap.fromList
    mapToList = HashMap.toList

    --findWithDefault = HashMap.findWithDefault
    insertWith = HashMap.insertWith
    --insertWithKey = HashMap.insertWithKey
    --insertLookupWithKey = HashMap.insertLookupWithKey
    adjustMap = HashMap.adjust
    --adjustWithKey = HashMap.adjustWithKey
    --updateMap = HashMap.update
    --updateWithKey = HashMap.updateWithKey
    --updateLookupWithKey = HashMap.updateLookupWithKey
    --alterMap = HashMap.alter
    unionWith = HashMap.unionWith
    --unionWithKey = HashMap.unionWithKey
    --unionsWith = HashMap.unionsWith
    --mapWithKey = HashMap.mapWithKey
    --mapKeysWith = HashMap.mapKeysWith

instance IsMap (IntMap.IntMap value) where
    type MapValue (IntMap.IntMap value) = value
    lookup = IntMap.lookup
    insertMap = IntMap.insert
    deleteMap = IntMap.delete
    singletonMap = IntMap.singleton
    mapFromList = IntMap.fromList
    mapToList = IntMap.toList

    findWithDefault = IntMap.findWithDefault
    insertWith = IntMap.insertWith
    insertWithKey = IntMap.insertWithKey
    insertLookupWithKey = IntMap.insertLookupWithKey
    adjustMap = IntMap.adjust
    adjustWithKey = IntMap.adjustWithKey
    updateMap = IntMap.update
    updateWithKey = IntMap.updateWithKey
    --updateLookupWithKey = IntMap.updateLookupWithKey
    alterMap = IntMap.alter
    unionWith = IntMap.unionWith
    unionWithKey = IntMap.unionWithKey
    unionsWith = IntMap.unionsWith
    mapWithKey = IntMap.mapWithKey
#if MIN_VERSION_containers(0, 5, 0)
    mapKeysWith = IntMap.mapKeysWith
#endif

instance Eq key => IsMap [(key, value)] where
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


-- | zip operations on MonoFunctors.
class MonoFunctor mono => MonoZip mono where
    ozipWith :: (Element mono -> Element mono -> Element mono) -> mono -> mono -> mono
    ozip :: mono -> mono -> [(Element mono, Element mono)]
    ounzip :: [(Element mono, Element mono)] -> (mono, mono)


instance MonoZip ByteString.ByteString where
    ozip     = ByteString.zip
    ounzip   = ByteString.unzip
    ozipWith f xs = ByteString.pack . ByteString.zipWith f xs
instance MonoZip LByteString.ByteString where
    ozip     = LByteString.zip
    ounzip   = LByteString.unzip
    ozipWith f xs = LByteString.pack . LByteString.zipWith f xs
instance MonoZip Text.Text where
    ozip     = Text.zip
    ounzip   = (Text.pack *** Text.pack) . List.unzip
    ozipWith = Text.zipWith
instance MonoZip LText.Text where
    ozip     = LText.zip
    ounzip   = (LText.pack *** LText.pack) . List.unzip
    ozipWith = LText.zipWith
