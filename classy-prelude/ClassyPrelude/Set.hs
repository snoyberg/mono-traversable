{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.Set
    ( Set
    ) where

import qualified Prelude
import Prelude ((.), Char)
import ClassyPrelude.Classes
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Monoid (Monoid)
import qualified Data.Monoid as Monoid
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable

instance (Prelude.Ord a, Prelude.Ord b) => CanMapFunc (Set a) (Set b) a b where
    mapFunc = Set.map
instance CanLength (Set x) Prelude.Int where
    length = Set.size
instance CanSingleton (Set x) x where
    singleton = Set.singleton
instance CanNull (Set x) where
    null = Set.null
instance Prelude.Ord x => CanPack (Set x) x where
    pack = Set.fromList
    unpack = Set.toList
instance (Prelude.Ord x, Set x ~ s, x ~ x') => CanInsert (x' -> s -> Set x) where
    insert = Set.insert
instance Prelude.Ord x => CanMember (Set x) x where
    member = Set.member

instance CanFoldFunc (Set a) a accum where
    foldFunc = Set.foldl'

instance Prelude.Ord a => CanMapM_Func (Set a) a where
    mapM_Func f = Prelude.mapM_ f . unpack

instance CanFind (Set a) a where
    find = Foldable.find

instance (Monoid m) => CanConcat (Set m) m where
    concat = Foldable.fold
