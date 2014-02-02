{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Data.MinLen
    ( -- * Type level naturals
      Zero (..)
    , Succ (..)
    , TypeNat (..)
      -- * Minimum length newtype wrapper
    , MinLen
    , unMinLen
    , toMinLenZero
    , toMinLen
    , mlcons
    , mlappend
    , head
    , last
    , tail
    , init
    , GrowingAppend
    ) where

import Prelude (Num (..), error, Maybe (..), Int, Ordering (..))
import Control.Category
import Data.MonoTraversable
import Data.Sequences
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Data.GrowingAppend

-- Type level naturals
data Zero = Zero
data Succ nat = Succ nat

class TypeNat nat where
    toValueNat :: Num i => nat -> i
instance TypeNat Zero where
    toValueNat Zero = 0
instance TypeNat nat => TypeNat (Succ nat) where
    toValueNat (Succ nat) = 1 + toValueNat nat

type family AddNat x y
type instance AddNat Zero y = y
type instance AddNat (Succ x) y = AddNat x (Succ y)

type family MaxNat x y
type instance MaxNat Zero y = y
type instance MaxNat x Zero = x
type instance MaxNat (Succ x) (Succ y) = Succ (MaxNat x y)

newtype MinLen nat mono = MinLen { unMinLen :: mono }

natProxy :: MinLen nat mono -> nat
natProxy = error "Data.MinLen.natProxy"

toMinLenZero :: mono -> MinLen Zero mono
toMinLenZero = MinLen

toMinLen :: (MonoFoldable mono, TypeNat nat) => mono -> Maybe (MinLen nat mono)
toMinLen mono =
    case ocompareLength mono (toValueNat nat :: Int) of
        LT -> Nothing
        _  -> Just res'
  where
    nat = natProxy res'
    res' = MinLen mono

mlcons :: IsSequence seq => Element seq -> MinLen nat seq -> MinLen (Succ nat) seq
mlcons e (MinLen seq) = MinLen (cons e seq)

mlappend :: IsSequence seq => MinLen x seq -> MinLen y seq -> MinLen (AddNat x y) seq
mlappend (MinLen x) (MinLen y) = MinLen (x `mappend` y)

head :: MonoTraversable mono => MinLen (Succ nat) mono -> Element mono
head = headEx . unMinLen

last :: MonoTraversable mono => MinLen (Succ nat) mono -> Element mono
last = lastEx . unMinLen

tail :: IsSequence seq => MinLen (Succ nat) seq -> MinLen nat seq
tail = MinLen . tailEx . unMinLen

init :: IsSequence seq => MinLen (Succ nat) seq -> MinLen nat seq
init = MinLen . initEx . unMinLen

instance GrowingAppend mono => Semigroup (MinLen nat mono) where
    MinLen x <> MinLen y = MinLen (x <> y)

mlunion :: GrowingAppend mono => MinLen x mono -> MinLen y mono -> MinLen (MaxNat x y) mono
mlunion (MinLen x) (MinLen y) = MinLen (x <> y)
