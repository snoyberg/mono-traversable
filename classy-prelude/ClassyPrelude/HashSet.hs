{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.HashSet
    ( HashSet
    ) where

import Prelude ()
import CorePrelude
import ClassyPrelude.Classes
import qualified Data.HashSet as HashSet
import qualified Control.Monad as Monad


instance (Eq b, Hashable b) => CanMapFunc (HashSet a) (HashSet b) a b where
    mapFunc = HashSet.map
    
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

instance CanFoldFunc (HashSet a) a accum where
    foldFunc = HashSet.foldl'

instance (Eq a, Hashable a) => CanMapM_Func (HashSet a) a where
    mapM_Func f = Monad.mapM_ f . unpack

instance (Eq a, Hashable a) => CanUnion (HashSet a) where
    union = HashSet.union

instance (Eq a, Hashable a) => CanDifference (HashSet a) where
    difference = HashSet.difference

instance (Eq a, Hashable a) => CanIntersection (HashSet a) where
    intersection = HashSet.intersection
