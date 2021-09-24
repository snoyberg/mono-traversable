{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
-- | Warning: This module should be considered highly experimental.
module Data.Containers where

import Prelude hiding (lookup)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import qualified Data.Set as Set
import qualified Data.HashSet as HashSet
import Data.Monoid (Monoid (..))
import Data.MonoTraversable (MonoFunctor(..), MonoFoldable, MonoTraversable, Element, GrowingAppend, ofoldl', otoList)
import Data.Function (on)
import qualified Data.List as List
import qualified Data.IntSet as IntSet

import qualified Data.Text.Lazy as LText
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.ByteString as ByteString
import Control.Arrow ((***))
import GHC.Exts (Constraint)

-- | A container whose values are stored in Key-Value pairs.
class (Data.Monoid.Monoid set, Semigroup set, MonoFoldable set, Eq (ContainerKey set), GrowingAppend set) => SetContainer set where
    -- | The type of the key
    type ContainerKey set

    -- | Check if there is a value with the supplied key
    -- in the container.
    member :: ContainerKey set -> set -> Bool

    -- | Check if there isn't a value with the supplied key
    -- in the container.
    notMember ::  ContainerKey set -> set -> Bool

    -- | Get the union of two containers.
    union :: set -> set -> set

    -- | Combine a collection of @SetContainer@s, with left-most values overriding
    -- when there are matching keys.
    --
    -- @since 1.0.0
    unions :: (MonoFoldable mono, Element mono ~ set) => mono -> set
    unions = ofoldl' union Data.Monoid.mempty
    {-# INLINE unions #-}

    -- | Get the difference of two containers.
    difference :: set -> set -> set

    -- | Get the intersection of two containers.
    intersection :: set -> set -> set

    -- | Get a list of all of the keys in the container.
    keys :: set -> [ContainerKey set]

-- | This instance uses the functions from "Data.Map.Strict".
instance Ord k => SetContainer (Map.Map k v) where
    type ContainerKey (Map.Map k v) = k
    member = Map.member
    {-# INLINE member #-}
    notMember = Map.notMember
    {-# INLINE notMember #-}
    union = Map.union
    {-# INLINE union #-}
    unions = Map.unions . otoList
    {-# INLINE unions #-}
    difference = Map.difference
    {-# INLINE difference #-}
    intersection = Map.intersection
    {-# INLINE intersection #-}
    keys = Map.keys
    {-# INLINE keys #-}

-- | This instance uses the functions from "Data.HashMap.Strict".
instance (Eq key, Hashable key) => SetContainer (HashMap.HashMap key value) where
    type ContainerKey (HashMap.HashMap key value) = key
    member = HashMap.member
    {-# INLINE member #-}
    notMember k = not . HashMap.member k
    {-# INLINE notMember #-}
    union = HashMap.union
    {-# INLINE union #-}
    unions = HashMap.unions . otoList
    {-# INLINE unions #-}
    difference = HashMap.difference
    {-# INLINE difference #-}
    intersection = HashMap.intersection
    {-# INLINE intersection #-}
    keys = HashMap.keys
    {-# INLINE keys #-}

-- | This instance uses the functions from "Data.IntMap.Strict".
instance SetContainer (IntMap.IntMap value) where
    type ContainerKey (IntMap.IntMap value) = Int
    member = IntMap.member
    {-# INLINE member #-}
    notMember = IntMap.notMember
    {-# INLINE notMember #-}
    union = IntMap.union
    {-# INLINE union #-}
    unions = IntMap.unions . otoList
    {-# INLINE unions #-}
    difference = IntMap.difference
    {-# INLINE difference #-}
    intersection = IntMap.intersection
    {-# INLINE intersection #-}
    keys = IntMap.keys
    {-# INLINE keys #-}

instance Ord element => SetContainer (Set.Set element) where
    type ContainerKey (Set.Set element) = element
    member = Set.member
    {-# INLINE member #-}
    notMember = Set.notMember
    {-# INLINE notMember #-}
    union = Set.union
    {-# INLINE union #-}
    unions = Set.unions . otoList
    {-# INLINE unions #-}
    difference = Set.difference
    {-# INLINE difference #-}
    intersection = Set.intersection
    {-# INLINE intersection #-}
    keys = Set.toList
    {-# INLINE keys #-}

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
    keys = HashSet.toList
    {-# INLINE keys #-}

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
    keys = IntSet.toList
    {-# INLINE keys #-}

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
    keys = map fst
    {-# INLINE keys #-}

-- | A guaranteed-polymorphic @Map@, which allows for more polymorphic versions
-- of functions.
class PolyMap map where
    -- | Get the difference between two maps, using the left map's values.
    differenceMap :: map value1 -> map value2 -> map value1
    {-
    differenceWithMap :: (value1 -> value2 -> Maybe value1)
                      -> map value1 -> map value2 -> map value1
    -}

    -- | Get the intersection of two maps, using the left map's values.
    intersectionMap :: map value1 -> map value2 -> map value1

    -- | Get the intersection of two maps with a supplied function
    -- that takes in the left map's value and the right map's value.
    intersectionWithMap :: (value1 -> value2 -> value3)
                        -> map value1 -> map value2 -> map value3

-- | This instance uses the functions from "Data.Map.Strict".
instance Ord key => PolyMap (Map.Map key) where
    differenceMap = Map.difference
    {-# INLINE differenceMap #-}
    --differenceWithMap = Map.differenceWith
    intersectionMap = Map.intersection
    {-# INLINE intersectionMap #-}
    intersectionWithMap = Map.intersectionWith
    {-# INLINE intersectionWithMap #-}

-- | This instance uses the functions from "Data.HashMap.Strict".
instance (Eq key, Hashable key) => PolyMap (HashMap.HashMap key) where
    differenceMap = HashMap.difference
    {-# INLINE differenceMap #-}
    --differenceWithMap = HashMap.differenceWith
    intersectionMap = HashMap.intersection
    {-# INLINE intersectionMap #-}
    intersectionWithMap = HashMap.intersectionWith
    {-# INLINE intersectionWithMap #-}

-- | This instance uses the functions from "Data.IntMap.Strict".
instance PolyMap IntMap.IntMap where
    differenceMap = IntMap.difference
    {-# INLINE differenceMap #-}
    --differenceWithMap = IntMap.differenceWith
    intersectionMap = IntMap.intersection
    {-# INLINE intersectionMap #-}
    intersectionWithMap = IntMap.intersectionWith
    {-# INLINE intersectionWithMap #-}

-- | A @Map@ type polymorphic in both its key and value.
class BiPolyMap map where
    type BPMKeyConstraint map key :: Constraint
    mapKeysWith :: (BPMKeyConstraint map k1, BPMKeyConstraint map k2)
                => (v -> v -> v) -- ^ combine values that now overlap
                -> (k1 -> k2)
                -> map k1 v
                -> map k2 v
instance BiPolyMap Map.Map where
    type BPMKeyConstraint Map.Map key = Ord key
    mapKeysWith = Map.mapKeysWith
    {-# INLINE mapKeysWith #-}
instance BiPolyMap HashMap.HashMap where
    type BPMKeyConstraint HashMap.HashMap key = (Hashable key, Eq key)
    mapKeysWith g f =
        mapFromList . unionsWith g . map go . mapToList
      where
        go (k, v) = [(f k, v)]
    {-# INLINE mapKeysWith #-}

-- | Polymorphic typeclass for interacting with different map types
class (MonoTraversable map, SetContainer map) => IsMap map where
    -- | In some cases, 'MapValue' and 'Element' will be different, e.g., the
    -- 'IsMap' instance of associated lists.
    type MapValue map

    -- | Look up a value in a map with a specified key.
    lookup       :: ContainerKey map -> map -> Maybe (MapValue map)

    -- | Insert a key-value pair into a map.
    insertMap    :: ContainerKey map -> MapValue map -> map -> map

    -- | Delete a key-value pair of a map using a specified key.
    deleteMap    :: ContainerKey map -> map -> map

    -- | Create a map from a single key-value pair.
    singletonMap :: ContainerKey map -> MapValue map -> map

    -- | Convert a list of key-value pairs to a map
    mapFromList  :: [(ContainerKey map, MapValue map)] -> map

    -- | Convert a map to a list of key-value pairs.
    mapToList    :: map -> [(ContainerKey map, MapValue map)]

    -- | Like 'lookup', but uses a default value when the key does
    -- not exist in the map.
    findWithDefault :: MapValue map -> ContainerKey map -> map -> MapValue map
    findWithDefault def key = fromMaybe def . lookup key

    -- | Insert a key-value pair into a map.
    --
    -- Inserts the value directly if the key does not exist in the map. Otherwise,
    -- apply a supplied function that accepts the new value and the previous value
    -- and insert that result into the map.
    insertWith :: (MapValue map -> MapValue map -> MapValue map)
                  -- ^ function that accepts the new value and the
                  -- previous value and returns the value that will be
                  -- set in the map.
               -> ContainerKey map -- ^ key
               -> MapValue map     -- ^ new value to insert
               -> map              -- ^ input map
               -> map              -- ^ resulting map
    insertWith f k v m =
        v' `seq` insertMap k v' m
      where
        v' =
            case lookup k m of
                Nothing -> v
                Just vold -> f v vold

    -- | Insert a key-value pair into a map.
    --
    -- Inserts the value directly if the key does not exist in the map. Otherwise,
    -- apply a supplied function that accepts the key, the new value, and the
    -- previous value and insert that result into the map.
    insertWithKey
        :: (ContainerKey map -> MapValue map -> MapValue map -> MapValue map)
           -- ^ function that accepts the key, the new value, and the
           -- previous value and returns the value that will be
           -- set in the map.
        -> ContainerKey map -- ^ key
        -> MapValue map     -- ^ new value to insert
        -> map              -- ^ input map
        -> map              -- ^ resulting map
    insertWithKey f k v m =
        v' `seq` insertMap k v' m
      where
        v' =
            case lookup k m of
                Nothing -> v
                Just vold -> f k v vold

    -- | Insert a key-value pair into a map, return the previous key's value
    -- if it existed.
    --
    -- Inserts the value directly if the key does not exist in the map. Otherwise,
    -- apply a supplied function that accepts the key, the new value, and the
    -- previous value and insert that result into the map.
    insertLookupWithKey
        :: (ContainerKey map -> MapValue map -> MapValue map -> MapValue map)
           -- ^ function that accepts the key, the new value, and the
           -- previous value and returns the value that will be
           -- set in the map.
        -> ContainerKey map            -- ^ key
        -> MapValue map                -- ^ new value to insert
        -> map                         -- ^ input map
        -> (Maybe (MapValue map), map) -- ^ previous value and the resulting map
    insertLookupWithKey f k v m =
        v' `seq` (mold, insertMap k v' m)
      where
        (mold, v') =
            case lookup k m of
                Nothing -> (Nothing, v)
                Just vold -> (Just vold, f k v vold)

    -- | Apply a function to the value of a given key.
    --
    -- Returns the input map when the key-value pair does not exist.
    adjustMap
        :: (MapValue map -> MapValue map)
           -- ^ function to apply to the previous value
        -> ContainerKey map -- ^ key
        -> map              -- ^ input map
        -> map              -- ^ resulting map
    adjustMap f k m =
        case lookup k m of
            Nothing -> m
            Just v ->
                let v' = f v
                 in v' `seq` insertMap k v' m

    -- | Equivalent to 'adjustMap', but the function accepts the key,
    -- as well as the previous value.
    adjustWithKey
        :: (ContainerKey map -> MapValue map -> MapValue map)
           -- ^ function that accepts the key and the previous value
           -- and returns the new value
        -> ContainerKey map -- ^ key
        -> map              -- ^ input map
        -> map              -- ^ resulting map
    adjustWithKey f k m =
        case lookup k m of
            Nothing -> m
            Just v ->
                let v' = f k v
                 in v' `seq` insertMap k v' m

    -- | Apply a function to the value of a given key.
    --
    -- If the function returns 'Nothing', this deletes the key-value pair.
    --
    -- Returns the input map when the key-value pair does not exist.
    updateMap
        :: (MapValue map -> Maybe (MapValue map))
           -- ^ function that accepts the previous value
           -- and returns the new value or 'Nothing'
        -> ContainerKey map -- ^ key
        -> map              -- ^ input map
        -> map              -- ^ resulting map
    updateMap f k m =
        case lookup k m of
            Nothing -> m
            Just v ->
                case f v of
                    Nothing -> deleteMap k m
                    Just v' -> v' `seq` insertMap k v' m

    -- | Equivalent to 'updateMap', but the function accepts the key,
    -- as well as the previous value.
    updateWithKey
        :: (ContainerKey map -> MapValue map -> Maybe (MapValue map))
           -- ^ function that accepts the key and the previous value
           -- and returns the new value or 'Nothing'
        -> ContainerKey map -- ^ key
        -> map              -- ^ input map
        -> map              -- ^ resulting map
    updateWithKey f k m =
        case lookup k m of
            Nothing -> m
            Just v ->
                case f k v of
                    Nothing -> deleteMap k m
                    Just v' -> v' `seq` insertMap k v' m

    -- | Apply a function to the value of a given key.
    --
    -- If the map does not contain the key this returns 'Nothing'
    -- and the input map.
    --
    -- If the map does contain the key but the function returns 'Nothing',
    -- this returns the previous value and the map with the key-value pair removed.
    --
    -- If the map contains the key and the function returns a value,
    -- this returns the new value and the map with the key-value pair with the new value.
    updateLookupWithKey
        :: (ContainerKey map -> MapValue map -> Maybe (MapValue map))
           -- ^ function that accepts the key and the previous value
           -- and returns the new value or 'Nothing'
        -> ContainerKey map            -- ^ key
        -> map                         -- ^ input map
        -> (Maybe (MapValue map), map) -- ^ previous/new value and the resulting map
    updateLookupWithKey f k m =
        case lookup k m of
            Nothing -> (Nothing, m)
            Just v ->
                case f k v of
                    Nothing -> (Just v, deleteMap k m)
                    Just v' -> v' `seq` (Just v', insertMap k v' m)

    -- | Update/Delete the value of a given key.
    --
    -- Applies a function to previous value of a given key, if it results in 'Nothing'
    -- delete the key-value pair from the map, otherwise replace the previous value
    -- with the new value.
    alterMap
        :: (Maybe (MapValue map) -> Maybe (MapValue map))
           -- ^ function that accepts the previous value and
           -- returns the new value or 'Nothing'
        -> ContainerKey map -- ^ key
        -> map              -- ^ input map
        -> map              -- ^ resulting map
    alterMap f k m =
        case f mold of
            Nothing ->
                case mold of
                    Nothing -> m
                    Just _ -> deleteMap k m
            Just v -> insertMap k v m
      where
        mold = lookup k m

    -- | Combine two maps.
    --
    -- When a key exists in both maps, apply a function
    -- to both of the values and use the result of that as the value
    -- of the key in the resulting map.
    unionWith
        :: (MapValue map -> MapValue map -> MapValue map)
           -- ^ function that accepts the first map's value and the second map's value
           -- and returns the new value that will be used
        -> map -- ^ first map
        -> map -- ^ second map
        -> map -- ^ resulting map
    unionWith f x y =
        mapFromList $ loop $ mapToList x ++ mapToList y
      where
        loop [] = []
        loop ((k, v):rest) =
            case List.lookup k rest of
                Nothing -> (k, v) : loop rest
                Just v' -> (k, f v v') : loop (deleteMap k rest)

    -- Equivalent to 'unionWith', but the function accepts the key,
    -- as well as both of the map's values.
    unionWithKey
        :: (ContainerKey map -> MapValue map -> MapValue map -> MapValue map)
           -- ^ function that accepts the key, the first map's value and the
           -- second map's value and returns the new value that will be used
        -> map -- ^ first map
        -> map -- ^ second map
        -> map -- ^ resulting map
    unionWithKey f x y =
        mapFromList $ loop $ mapToList x ++ mapToList y
      where
        loop [] = []
        loop ((k, v):rest) =
            case List.lookup k rest of
                Nothing -> (k, v) : loop rest
                Just v' -> (k, f k v v') : loop (deleteMap k rest)

    -- | Combine a list of maps.
    --
    -- When a key exists in two different maps, apply a function
    -- to both of the values and use the result of that as the value
    -- of the key in the resulting map.
    unionsWith
        :: (MapValue map -> MapValue map -> MapValue map)
           -- ^ function that accepts the first map's value and the second map's value
           -- and returns the new value that will be used
        -> [map] -- ^ input list of maps
        -> map   -- ^ resulting map
    unionsWith _ [] = mempty
    unionsWith _ [x] = x
    unionsWith f (x:y:z) = unionsWith f (unionWith f x y:z)

    -- | Apply a function over every key-value pair of a map.
    mapWithKey
        :: (ContainerKey map -> MapValue map -> MapValue map)
           -- ^ function that accepts the key and the previous value
           -- and returns the new value
        -> map -- ^ input map
        -> map -- ^ resulting map
    mapWithKey f =
        mapFromList . map go . mapToList
      where
        go (k, v) = (k, f k v)

    -- | Apply a function over every key of a pair and run
    -- 'unionsWith' over the results.
    omapKeysWith
        :: (MapValue map -> MapValue map -> MapValue map)
           -- ^ function that accepts the first map's value and the second map's value
           -- and returns the new value that will be used
        -> (ContainerKey map -> ContainerKey map)
           -- ^ function that accepts the previous key and
           -- returns the new key
        -> map -- ^ input map
        -> map -- ^ resulting map
    omapKeysWith g f =
        mapFromList . unionsWith g . map go . mapToList
      where
        go (k, v) = [(f k, v)]

    -- | Filter values in a map.
    --
    -- @since 1.0.9.0
    filterMap :: IsMap map => (MapValue map -> Bool) -> map -> map
    filterMap p = mapFromList . filter (p . snd) . mapToList

-- | This instance uses the functions from "Data.Map.Strict".
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
    omapKeysWith = Map.mapKeysWith
    {-# INLINE omapKeysWith #-}
    filterMap = Map.filter
    {-# INLINE filterMap #-}

-- | This instance uses the functions from "Data.HashMap.Strict".
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
    filterMap = HashMap.filter
    {-# INLINE filterMap #-}

-- | This instance uses the functions from "Data.IntMap.Strict".
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
    omapKeysWith = IntMap.mapKeysWith
    {-# INLINE omapKeysWith #-}
    filterMap = IntMap.filter
    {-# INLINE filterMap #-}

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

-- | Polymorphic typeclass for interacting with different set types
class (SetContainer set, Element set ~ ContainerKey set) => IsSet set where
    -- | Insert a value into a set.
    insertSet :: Element set -> set -> set

    -- | Delete a value from a set.
    deleteSet :: Element set -> set -> set

    -- | Create a set from a single element.
    singletonSet :: Element set -> set

    -- | Convert a list to a set.
    setFromList :: [Element set] -> set

    -- | Convert a set to a list.
    setToList :: set -> [Element set]

    -- | Filter values in a set.
    --
    -- @since 1.0.12.0
    filterSet :: (Element set -> Bool) -> set -> set
    filterSet p = setFromList . filter p . setToList

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
    filterSet = Set.filter
    {-# INLINE filterSet #-}

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
    filterSet = HashSet.filter
    {-# INLINE filterSet #-}

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
    filterSet = IntSet.filter
    {-# INLINE filterSet #-}


-- | Zip operations on 'MonoFunctor's.
class MonoFunctor mono => MonoZip mono where
    -- | Combine each element of two 'MonoZip's using a supplied function.
    ozipWith :: (Element mono -> Element mono -> Element mono) -> mono -> mono -> mono

    -- | Take two 'MonoZip's and return a list of the pairs of their elements.
    ozip :: mono -> mono -> [(Element mono, Element mono)]

    -- | Take a list of pairs of elements and return a 'MonoZip' of the first
    -- components and a 'MonoZip' of the second components.
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

-- | Type class for maps whose keys can be converted into sets.
class SetContainer set => HasKeysSet set where
    -- | Type of the key set.
    type KeySet set

    -- | Convert a map into a set of its keys.
    keysSet :: set -> KeySet set

instance Ord k => HasKeysSet (Map.Map k v) where
    type KeySet (Map.Map k v) = Set.Set k
    keysSet = Map.keysSet
instance HasKeysSet (IntMap.IntMap v) where
    type KeySet (IntMap.IntMap v) = IntSet.IntSet
    keysSet = IntMap.keysSet
instance (Hashable k, Eq k) => HasKeysSet (HashMap.HashMap k v) where
    type KeySet (HashMap.HashMap k v) = HashSet.HashSet k
    keysSet = setFromList . HashMap.keys
