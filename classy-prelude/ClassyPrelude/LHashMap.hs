{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.LHashMap
    ( LHashMap
    ) where

import qualified Prelude
import Prelude ((.), Char)
import ClassyPrelude.Classes
import qualified Data.HashMap.Lazy as Map
import Data.Hashable (Hashable)

type LHashMap = Map.HashMap
type Map = LHashMap

{-
instance CanMapFunc (Map k v1) (Map k v2) v1 v2 where
    mapFunc = Map.map
instance Hashable k => CanFilterFunc (Map k v) (k, v) where
    filterFunc = Map.filterWithKey . Prelude.curry
instance CanLength (Map k v) Prelude.Int where
    length = Map.size
instance (Prelude.Eq k, Hashable k, v' ~ v) => CanSingleton (v' -> Map k v) k where
    singleton = Map.singleton
instance CanNull (Map k v) where
    null = Map.null
instance (Prelude.Eq k, Hashable k) => CanPack (Map k v) (k, v) where
    pack = Map.fromList
    unpack = Map.toList
instance (Prelude.Eq k, Hashable k) => CanLookup (Map k v) k v where
    lookup = Map.lookup
instance CanEmpty (Map k v) where
    empty = Map.empty
instance (Prelude.Eq k, Hashable k) => CanInsertVal (Map k v) k v where
    insertVal = Map.insert
instance (Prelude.Eq k, Hashable k) => CanDelete (Map k v) k where
    delete = Map.delete
    -}
