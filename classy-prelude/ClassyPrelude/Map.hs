{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.Map
    ( Map
    ) where

import ClassyPrelude.Classes
import Prelude ((.), ($), otherwise, Maybe(..), Monad, Ord, Eq, Int, Bool, Char, Bool(..))
import Data.Monoid (Monoid)
import Data.Map (Map)
import qualified Prelude
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map


instance CanMapFunc (Map k v1) (Map k v2) v1 v2 where
    mapFunc = Map.map

instance Ord k => CanFilterFunc (Map k v) (Map k v) (k, v) where
    filterFunc = Map.filterWithKey . Prelude.curry

instance CanLength (Map k v) Int where
    length = Map.size

instance (v' ~ v) => CanSingleton (v' -> Map k v) k where
    singleton = Map.singleton

instance CanNull (Map k v) where
    null = Map.null

instance Ord k => CanPack (Map k v) (k, v) where
    pack = Map.fromList
    unpack = Map.toList

instance Ord k => CanLookup (Map k v) k v where
    lookup = Map.lookup

instance Ord k => CanInsertVal (Map k v) k v where
    insertVal = Map.insert

instance Ord k => CanDeleteVal (Map k v) k where
    deleteVal = Map.delete

instance CanFind (Map k v) v where
    find = Foldable.find

instance (Monoid v) => CanConcat (Map k v) v where
    concat = Foldable.fold

instance Ord k => CanPartition (Map k v) v where
    partition = Map.partition
