{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.Set
    ( Set
    ) where

import ClassyPrelude.Classes
import Prelude ((.), ($), otherwise, Maybe(..), Monad, Ord, Eq, Int, Bool, Char, Bool(..))
import Data.Monoid (Monoid)
import Data.Set (Set)
import qualified Prelude
import qualified Data.Foldable as Foldable
import qualified Data.Set as Set


instance (Ord a, Ord b) => CanMapFunc (Set a) (Set b) a b where
    mapFunc = Set.map

instance CanLength (Set x) Int where
    length = Set.size

instance CanSingleton (Set x) x where
    singleton = Set.singleton

instance CanNull (Set x) where
    null = Set.null

instance Ord x => CanPack (Set x) x where
    pack = Set.fromList
    unpack = Set.toList

instance (Ord x, Set x ~ s, x ~ x') => CanInsert (x' -> s -> Set x) where
    insert = Set.insert

instance Ord x => CanMember (Set x) x where
    member = Set.member

instance CanFoldFunc (Set a) a accum where
    foldFunc = Set.foldl'

instance Ord a => CanMapM_Func (Set a) a where
    mapM_Func f = Prelude.mapM_ f . unpack

instance CanFind (Set a) a where
    find = Foldable.find

instance (Monoid m) => CanConcat (Set m) m where
    concat = Foldable.fold

instance (Ord a) => CanPartition (Set a) a where
    partition = Set.partition
