{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.HashMap
    ( HashMap
    ) where

import qualified Prelude
import Prelude ((.), Char)
import ClassyPrelude.Classes
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Hashable (Hashable)

type Map = HashMap

instance (v1 ~ v1', co ~ Map k v2) => CanMapFunc (Map k v1) co v1' v2 where
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
