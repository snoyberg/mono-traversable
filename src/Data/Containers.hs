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
import qualified Data.IntMap.Strict as IntMap
#else
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
#endif
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import qualified Data.Set as Set
import qualified Data.HashSet as HashSet
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup)
import Data.MonoTraversable (MonoFunctor(..), MonoFoldable, MonoTraversable, Element)
import Data.Function (on)
import qualified Data.List as List
import qualified Data.IntSet as IntSet

import qualified Data.Text.Lazy as LText
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.ByteString as ByteString
import Control.Arrow ((***))
import Data.GrowingAppend

class (Monoid set, Semigroup set, MonoFoldable set, Eq (ContainerKey set), GrowingAppend set) => SetContainer set where
    type ContainerKey set
    member :: ContainerKey set -> set -> Bool
    notMember ::  ContainerKey set -> set -> Bool
    union :: set -> set -> set
    difference :: set -> set -> set
    intersection :: set -> set -> set

#if MIN_VERSION_containers(0, 5, 0)
-- | This instance uses the functions from "Data.Map.Strict".
#endif
instance Ord k => SetContainer (Map.Map k v) where
    type ContainerKey (Map.Map k v) = k
    member = Map.member
    {-# INLINE member #-}
    notMember = Map.notMember
    {-# INLINE notMember #-}
    union = Map.union
    {-# INLINE union #-}
    difference = Map.difference
    {-# INLINE difference #-}
    intersection = Map.intersection
    {-# INLINE intersection #-}

#if MIN_VERSION_containers(0, 5, 0)
-- | This instance uses the functions from "Data.HashMap.Strict".
#endif
instance (Eq key, Hashable key) => SetContainer (HashMap.HashMap key value) where
    type ContainerKey (HashMap.HashMap key value) = key
    member = HashMap.member
    {-# INLINE member #-}
    notMember k = not . HashMap.member k
    {-# INLINE notMember #-}
    union = HashMap.union
    {-# INLINE union #-}
    difference = HashMap.difference
    {-# INLINE difference #-}
    intersection = HashMap.intersection
    {-# INLINE intersection #-}

#if MIN_VERSION_containers(0, 5, 0)
-- | This instance uses the functions from "Data.IntMap.Strict".
#endif
instance SetContainer (IntMap.IntMap value) where
    type ContainerKey (IntMap.IntMap value) = Int
    member = IntMap.member
    {-# INLINE member #-}
    notMember = IntMap.notMember
    {-# INLINE notMember #-}
    union = IntMap.union
    {-# INLINE union #-}
    difference = IntMap.difference
    {-# INLINE difference #-}
    intersection = IntMap.intersection
    {-# INLINE intersection #-}

instance Ord element => SetContainer (Set.Set element) where
    type ContainerKey (Set.Set element) = element
    member = Set.member
    {-# INLINE member #-}
    notMember = Set.notMember
    {-# INLINE notMember #-}
    union = Set.union
    {-# INLINE union #-}
    difference = Set.difference
    {-# INLINE difference #-}
    intersection = Set.intersection
    {-# INLINE intersection #-}

instance (Eq element, Hashable element) => SetContainer (HashSet.HashSet element) where
    type ContainerKey (HashSet.HashSet element) = element
    member = HashSet.member
    {-# INLINE member #-}
    notMember e = not . HashSet.member e
    {-# INLINE notMember #-}
    union = HashSet.union
    {-# INLINE union #-}
    difference = HashSet.difference
    {-# INLINE difference #-}
    intersection = HashSet.intersection
    {-# INLINE intersection #-}

instance SetContainer IntSet.IntSet where
    type ContainerKey IntSet.IntSet = Int
    member = IntSet.member
    {-# INLINE member #-}
    notMember = IntSet.notMember
    {-# INLINE notMember #-}
    union = IntSet.union
    {-# INLINE union #-}
    difference = IntSet.difference
    {-# INLINE difference #-}
    intersection = IntSet.intersection
    {-# INLINE intersection #-}

instance Eq key => SetContainer [(key, value)] where
    type ContainerKey [(key, value)] = key
    member k = List.any ((== k) . fst)
    {-# INLINE member #-}
    notMember k = not . member k
    {-# INLINE notMember #-}
    union = List.unionBy ((==) `on` fst)
    {-# INLINE union #-}
    x `difference` y =
        loop x
      where
        loop [] = []
        loop ((k, v):rest) =
            case lookup k y of
                Nothing -> (k, v) : loop rest
                Just _ -> loop rest
    intersection = List.intersectBy ((==) `on` fst)
    {-# INLINE intersection #-}

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

#if MIN_VERSION_containers(0, 5, 0)
-- | This instance uses the functions from "Data.Map.Strict".
#endif
instance Ord key => PolyMap (Map.Map key) where
    differenceMap = Map.difference
    {-# INLINE differenceMap #-}
    --differenceWithMap = Map.differenceWith
    intersectionMap = Map.intersection
    {-# INLINE intersectionMap #-}
    intersectionWithMap = Map.intersectionWith
    {-# INLINE intersectionWithMap #-}

#if MIN_VERSION_containers(0, 5, 0)
-- | This instance uses the functions from "Data.HashMap.Strict".
#endif
instance (Eq key, Hashable key) => PolyMap (HashMap.HashMap key) where
    differenceMap = HashMap.difference
    {-# INLINE differenceMap #-}
    --differenceWithMap = HashMap.differenceWith
    intersectionMap = HashMap.intersection
    {-# INLINE intersectionMap #-}
    intersectionWithMap = HashMap.intersectionWith
    {-# INLINE intersectionWithMap #-}

#if MIN_VERSION_containers(0, 5, 0)
-- | This instance uses the functions from "Data.IntMap.Strict".
#endif
instance PolyMap IntMap.IntMap where
    differenceMap = IntMap.difference
    {-# INLINE differenceMap #-}
    --differenceWithMap = IntMap.differenceWith
    intersectionMap = IntMap.intersection
    {-# INLINE intersectionMap #-}
    intersectionWithMap = IntMap.intersectionWith
    {-# INLINE intersectionWithMap #-}

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

#if MIN_VERSION_containers(0, 5, 0)
-- | This instance uses the functions from "Data.Map.Strict".
#endif
instance Ord key => IsMap (Map.Map key value) where
    type MapValue (Map.Map key value) = value
    lookup = Map.lookup
    {-# INLINE lookup #-}
    insertMap = Map.insert
    {-# INLINE insertMap #-}
    deleteMap = Map.delete
    {-# INLINE deleteMap #-}
    singletonMap = Map.singleton
    {-# INLINE singletonMap #-}
    mapFromList = Map.fromList
    {-# INLINE mapFromList #-}
    mapToList = Map.toList
    {-# INLINE mapToList #-}

    findWithDefault = Map.findWithDefault
    {-# INLINE findWithDefault #-}
    insertWith = Map.insertWith
    {-# INLINE insertWith #-}
    insertWithKey = Map.insertWithKey
    {-# INLINE insertWithKey #-}
    insertLookupWithKey = Map.insertLookupWithKey
    {-# INLINE insertLookupWithKey #-}
    adjustMap = Map.adjust
    {-# INLINE adjustMap #-}
    adjustWithKey = Map.adjustWithKey
    {-# INLINE adjustWithKey #-}
    updateMap = Map.update
    {-# INLINE updateMap #-}
    updateWithKey = Map.updateWithKey
    {-# INLINE updateWithKey #-}
    updateLookupWithKey = Map.updateLookupWithKey
    {-# INLINE updateLookupWithKey #-}
    alterMap = Map.alter
    {-# INLINE alterMap #-}
    unionWith = Map.unionWith
    {-# INLINE unionWith #-}
    unionWithKey = Map.unionWithKey
    {-# INLINE unionWithKey #-}
    unionsWith = Map.unionsWith
    {-# INLINE unionsWith #-}
    mapWithKey = Map.mapWithKey
    {-# INLINE mapWithKey #-}
    mapKeysWith = Map.mapKeysWith
    {-# INLINE mapKeysWith #-}

#if MIN_VERSION_containers(0, 5, 0)
-- | This instance uses the functions from "Data.HashMap.Strict".
#endif
instance (Eq key, Hashable key) => IsMap (HashMap.HashMap key value) where
    type MapValue (HashMap.HashMap key value) = value
    lookup = HashMap.lookup
    {-# INLINE lookup #-}
    insertMap = HashMap.insert
    {-# INLINE insertMap #-}
    deleteMap = HashMap.delete
    {-# INLINE deleteMap #-}
    singletonMap = HashMap.singleton
    {-# INLINE singletonMap #-}
    mapFromList = HashMap.fromList
    {-# INLINE mapFromList #-}
    mapToList = HashMap.toList
    {-# INLINE mapToList #-}

    --findWithDefault = HashMap.findWithDefault
    insertWith = HashMap.insertWith
    {-# INLINE insertWith #-}
    --insertWithKey = HashMap.insertWithKey
    --insertLookupWithKey = HashMap.insertLookupWithKey
    adjustMap = HashMap.adjust
    {-# INLINE adjustMap #-}
    --adjustWithKey = HashMap.adjustWithKey
    --updateMap = HashMap.update
    --updateWithKey = HashMap.updateWithKey
    --updateLookupWithKey = HashMap.updateLookupWithKey
    --alterMap = HashMap.alter
    unionWith = HashMap.unionWith
    {-# INLINE unionWith #-}
    --unionWithKey = HashMap.unionWithKey
    --unionsWith = HashMap.unionsWith
    --mapWithKey = HashMap.mapWithKey
    --mapKeysWith = HashMap.mapKeysWith

#if MIN_VERSION_containers(0, 5, 0)
-- | This instance uses the functions from "Data.IntMap.Strict".
#endif
instance IsMap (IntMap.IntMap value) where
    type MapValue (IntMap.IntMap value) = value
    lookup = IntMap.lookup
    {-# INLINE lookup #-}
    insertMap = IntMap.insert
    {-# INLINE insertMap #-}
    deleteMap = IntMap.delete
    {-# INLINE deleteMap #-}
    singletonMap = IntMap.singleton
    {-# INLINE singletonMap #-}
    mapFromList = IntMap.fromList
    {-# INLINE mapFromList #-}
    mapToList = IntMap.toList
    {-# INLINE mapToList #-}

    findWithDefault = IntMap.findWithDefault
    {-# INLINE findWithDefault #-}
    insertWith = IntMap.insertWith
    {-# INLINE insertWith #-}
    insertWithKey = IntMap.insertWithKey
    {-# INLINE insertWithKey #-}
    insertLookupWithKey = IntMap.insertLookupWithKey
    {-# INLINE insertLookupWithKey #-}
    adjustMap = IntMap.adjust
    {-# INLINE adjustMap #-}
    adjustWithKey = IntMap.adjustWithKey
    {-# INLINE adjustWithKey #-}
    updateMap = IntMap.update
    {-# INLINE updateMap #-}
    updateWithKey = IntMap.updateWithKey
    {-# INLINE updateWithKey #-}
    --updateLookupWithKey = IntMap.updateLookupWithKey
    alterMap = IntMap.alter
    {-# INLINE alterMap #-}
    unionWith = IntMap.unionWith
    {-# INLINE unionWith #-}
    unionWithKey = IntMap.unionWithKey
    {-# INLINE unionWithKey #-}
    unionsWith = IntMap.unionsWith
    {-# INLINE unionsWith #-}
    mapWithKey = IntMap.mapWithKey
    {-# INLINE mapWithKey #-}
#if MIN_VERSION_containers(0, 5, 0)
    mapKeysWith = IntMap.mapKeysWith
    {-# INLINE mapKeysWith #-}
#endif

instance Eq key => IsMap [(key, value)] where
    type MapValue [(key, value)] = value
    lookup = List.lookup
    {-# INLINE lookup #-}
    insertMap k v = ((k, v):) . deleteMap k
    {-# INLINE insertMap #-}
    deleteMap k = List.filter ((/= k) . fst)
    {-# INLINE deleteMap #-}
    singletonMap k v = [(k, v)]
    {-# INLINE singletonMap #-}
    mapFromList = id
    {-# INLINE mapFromList #-}
    mapToList = id
    {-# INLINE mapToList #-}

class (SetContainer set, Element set ~ ContainerKey set) => IsSet set where
    insertSet :: Element set -> set -> set
    deleteSet :: Element set -> set -> set
    singletonSet :: Element set -> set
    setFromList :: [Element set] -> set
    setToList :: set -> [Element set]

instance Ord element => IsSet (Set.Set element) where
    insertSet = Set.insert
    {-# INLINE insertSet #-}
    deleteSet = Set.delete
    {-# INLINE deleteSet #-}
    singletonSet = Set.singleton
    {-# INLINE singletonSet #-}
    setFromList = Set.fromList
    {-# INLINE setFromList #-}
    setToList = Set.toList
    {-# INLINE setToList #-}

instance (Eq element, Hashable element) => IsSet (HashSet.HashSet element) where
    insertSet = HashSet.insert
    {-# INLINE insertSet #-}
    deleteSet = HashSet.delete
    {-# INLINE deleteSet #-}
    singletonSet = HashSet.singleton
    {-# INLINE singletonSet #-}
    setFromList = HashSet.fromList
    {-# INLINE setFromList #-}
    setToList = HashSet.toList
    {-# INLINE setToList #-}

instance IsSet IntSet.IntSet where
    insertSet = IntSet.insert
    {-# INLINE insertSet #-}
    deleteSet = IntSet.delete
    {-# INLINE deleteSet #-}
    singletonSet = IntSet.singleton
    {-# INLINE singletonSet #-}
    setFromList = IntSet.fromList
    {-# INLINE setFromList #-}
    setToList = IntSet.toList
    {-# INLINE setToList #-}


-- | zip operations on MonoFunctors.
class MonoFunctor mono => MonoZip mono where
    ozipWith :: (Element mono -> Element mono -> Element mono) -> mono -> mono -> mono
    ozip :: mono -> mono -> [(Element mono, Element mono)]
    ounzip :: [(Element mono, Element mono)] -> (mono, mono)


instance MonoZip ByteString.ByteString where
    ozip     = ByteString.zip
    ounzip   = ByteString.unzip
    ozipWith f xs = ByteString.pack . ByteString.zipWith f xs
    {-# INLINE ozip #-}
    {-# INLINE ounzip #-}
    {-# INLINE ozipWith #-}
instance MonoZip LByteString.ByteString where
    ozip     = LByteString.zip
    ounzip   = LByteString.unzip
    ozipWith f xs = LByteString.pack . LByteString.zipWith f xs
    {-# INLINE ozip #-}
    {-# INLINE ounzip #-}
    {-# INLINE ozipWith #-}
instance MonoZip Text.Text where
    ozip     = Text.zip
    ounzip   = (Text.pack *** Text.pack) . List.unzip
    ozipWith = Text.zipWith
    {-# INLINE ozip #-}
    {-# INLINE ounzip #-}
    {-# INLINE ozipWith #-}
instance MonoZip LText.Text where
    ozip     = LText.zip
    ounzip   = (LText.pack *** LText.pack) . List.unzip
    ozipWith = LText.zipWith
    {-# INLINE ozip #-}
    {-# INLINE ounzip #-}
    {-# INLINE ozipWith #-}
