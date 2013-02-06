{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.HashSet
    ( HashSet
    ) where

import CorePrelude
import ClassyPrelude.Classes
import qualified Data.HashSet as HashSet
import qualified Control.Monad as Monad


instance (Eq b, Hashable b) => CanMap (HashSet a) (HashSet b) a b where
    map = HashSet.map
    
instance CanLength (HashSet x) Int where
    length = HashSet.size

instance Hashable x => CanSingleton (HashSet x) x where
    singleton = HashSet.singleton

instance CanNull (HashSet x) where
    null = HashSet.null

instance (Hashable x, Eq x) => CanPack (HashSet x) x where
    pack = HashSet.fromList
    unpack = HashSet.toList

instance (Eq x, Hashable x, HashSet x ~ s, x ~ x') => CanInsert (x' -> s -> HashSet x) where
    insert = HashSet.insert

instance (Eq x, Hashable x) => CanMember (HashSet x) x where
    member = HashSet.member

instance CanFold (HashSet a) a accum where
    fold = HashSet.foldl'

instance (Eq a, Hashable a) => CanMapM_ (HashSet a) a where
    mapM_ f = Monad.mapM_ f . unpack

instance (Eq a, Hashable a) => CanUnion (HashSet a) where
    union = HashSet.union

instance (Eq a, Hashable a) => CanDifference (HashSet a) where
    difference = HashSet.difference

instance (Eq a, Hashable a) => CanIntersection (HashSet a) where
    intersection = HashSet.intersection
