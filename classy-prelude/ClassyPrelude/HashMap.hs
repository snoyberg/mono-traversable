{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.HashMap
    ( HashMap
    ) where

import ClassyPrelude.Classes
import Prelude ((.), ($), otherwise, Maybe(..), Monad, Ord, Eq, Int, Bool, Char, Bool(..))
import Data.Monoid (Monoid)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Prelude
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as Map


type Map = HashMap

instance CanMapFunc (Map k v1) (Map k v2) v1 v2 where
    mapFunc = Map.map
    
instance Hashable k => CanFilterFunc (Map k v) (Map k v) (k, v) where
    filterFunc = Map.filterWithKey . Prelude.curry
    
instance CanLength (Map k v) Int where
    length = Map.size
    
instance (Eq k, Hashable k, v' ~ v) => CanSingleton (v' -> Map k v) k where
    singleton = Map.singleton
    
instance CanNull (Map k v) where
    null = Map.null
    
instance (Eq k, Hashable k) => CanPack (Map k v) (k, v) where
    pack = Map.fromList
    unpack = Map.toList
    
instance (Eq k, Hashable k) => CanLookup (Map k v) k v where
    lookup = Map.lookup
    
instance (Eq k, Hashable k) => CanInsertVal (Map k v) k v where
    insertVal = Map.insert
    
instance (Eq k, Hashable k) => CanDeleteVal (Map k v) k where
    deleteVal = Map.delete
    
instance CanFind (Map k v) v where
    find = Foldable.find
    
instance (Monoid v) => CanConcat (Map k v) v where
    concat = Foldable.fold
    
instance Hashable k => CanPartition (Map k v) v where
    partition p m = (Map.filter p m, Map.filter (Prelude.not . p) m)
