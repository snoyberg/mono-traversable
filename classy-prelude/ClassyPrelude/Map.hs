{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.Map
    ( Map
    ) where

import qualified Prelude
import Prelude ((.), Char)
import ClassyPrelude.Classes
import Data.Map (Map)
import qualified Data.Map as Map

instance (v1 ~ v1', co ~ Map k v2) => CanMapFunc (Map k v1) co v1' v2 where
    mapFunc = Map.map
instance Prelude.Ord k => CanFilterFunc (Map k v) (k, v) where
    filterFunc = Map.filterWithKey . Prelude.curry
instance CanLength (Map k v) Prelude.Int where
    length = Map.size
instance (v' ~ v) => CanSingleton (v' -> Map k v) k where
    singleton = Map.singleton
instance CanNull (Map k v) where
    null = Map.null
instance Prelude.Ord k => CanPack (Map k v) (k, v) where
    pack = Map.fromList
    unpack = Map.toList
instance Prelude.Ord k => CanLookup (Map k v) k v where
    lookup = Map.lookup
instance CanEmpty (Map k v) where
    empty = Map.empty
instance Prelude.Ord k => CanInsertVal (Map k v) k v where
    insertVal = Map.insert
instance Prelude.Ord k => CanDelete (Map k v) k where
    delete = Map.delete
