{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.MinLen
    ( -- * Type level naturals
      Zero (..)
    , Succ (..)
    , TypeNat (..)
    , AddNat
    , MaxNat
      -- * Minimum length newtype wrapper
    , MinLen
    , unMinLen
    , toMinLenZero
    , toMinLen
    , unsafeToMinLen
    , mlcons
    , mlappend
    , mlunion
    , head
    , last
    , tailML
    , initML
    , GrowingAppend
    , ofoldMap1
    , ofold1
    , ofoldr1
    , ofoldl1'
    , maximum
    , minimum
    , maximumBy
    , minimumBy
    ) where

import Prelude (Num (..), Maybe (..), Int, Ordering (..), Eq, Ord, Read, Show, Functor (..), ($), flip)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Control.Category
import Data.MonoTraversable
import Data.Sequences
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Data.GrowingAppend
import Control.Monad (liftM)

-- Type level naturals
data Zero = Zero
data Succ nat = Succ nat

class TypeNat nat where
    toValueNat :: Num i => nat -> i
    typeNat :: nat
instance TypeNat Zero where
    toValueNat Zero = 0
    typeNat = Zero
instance TypeNat nat => TypeNat (Succ nat) where
    toValueNat (Succ nat) = 1 + toValueNat nat
    typeNat = Succ typeNat

type family AddNat x y
type instance AddNat Zero y = y
type instance AddNat (Succ x) y = AddNat x (Succ y)

type family MaxNat x y
type instance MaxNat Zero y = y
type instance MaxNat x Zero = x
type instance MaxNat (Succ x) (Succ y) = Succ (MaxNat x y)

newtype MinLen nat mono = MinLen { unMinLen :: mono }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Functor)
type instance Element (MinLen nat mono) = Element mono
deriving instance MonoFunctor mono => MonoFunctor (MinLen nat mono)
deriving instance MonoFoldable mono => MonoFoldable (MinLen nat mono)
deriving instance MonoFoldableOrd mono => MonoFoldableOrd (MinLen nat mono)
instance MonoTraversable mono => MonoTraversable (MinLen nat mono) where
    otraverse f (MinLen x) = fmap MinLen (otraverse f x)
    {-# INLINE otraverse #-}
    omapM f (MinLen x) = liftM MinLen (omapM f x)
    {-# INLINE omapM #-}
deriving instance GrowingAppend mono => GrowingAppend (MinLen nat mono)


instance GrowingAppend mono => Semigroup (MinLen nat mono) where
    MinLen x <> MinLen y = MinLen (x <> y)

instance SemiSequence seq => SemiSequence (MinLen nat seq) where
    type Index (MinLen nat seq) = Index seq

    intersperse e = fmap $ intersperse e
    reverse       = fmap reverse
    find f        = find f . unMinLen
    cons x        = fmap $ cons x
    snoc xs x     = fmap (flip snoc x) xs
    sortBy f      = fmap $ sortBy f

instance MonoPointed mono => MonoPointed (MinLen Zero mono) where
    opoint = MinLen . opoint
    {-# INLINE opoint #-}
instance MonoPointed mono => MonoPointed (MinLen (Succ Zero) mono) where
    opoint = MinLen . opoint
    {-# INLINE opoint #-}

natProxy :: TypeNat nat => MinLen nat mono -> nat
natProxy _ = typeNat

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

-- | Although this function itself cannot cause a segfault, it breaks the
-- safety guarantees of @MinLen@ and can lead to a segfault when using
-- otherwise safe functions.
unsafeToMinLen :: mono -> MinLen nat mono
unsafeToMinLen = MinLen

infixr 5 `mlcons`

mlcons :: IsSequence seq => Element seq -> MinLen nat seq -> MinLen (Succ nat) seq
mlcons e (MinLen seq) = MinLen (cons e seq)
{-# INLINE mlcons #-}

mlappend :: IsSequence seq => MinLen x seq -> MinLen y seq -> MinLen (AddNat x y) seq
mlappend (MinLen x) (MinLen y) = MinLen (x `mappend` y)
{-# INLINE mlappend #-}

head :: MonoTraversable mono => MinLen (Succ nat) mono -> Element mono
head = headEx . unMinLen
{-# INLINE head #-}

last :: MonoTraversable mono => MinLen (Succ nat) mono -> Element mono
last = lastEx . unMinLen
{-# INLINE last #-}

tailML :: IsSequence seq => MinLen (Succ nat) seq -> MinLen nat seq
tailML = MinLen . tailEx . unMinLen

initML :: IsSequence seq => MinLen (Succ nat) seq -> MinLen nat seq
initML = MinLen . initEx . unMinLen

mlunion :: GrowingAppend mono => MinLen x mono -> MinLen y mono -> MinLen (MaxNat x y) mono
mlunion (MinLen x) (MinLen y) = MinLen (x <> y)


ofoldMap1 :: (MonoFoldable mono, Semigroup m) => (Element mono -> m) -> MinLen (Succ nat) mono -> m
ofoldMap1 f = ofoldMap1Ex f . unMinLen
{-# INLINE ofoldMap1 #-}

ofold1 :: (MonoFoldable mono, Semigroup (Element mono)) => MinLen (Succ nat) mono -> Element mono
ofold1 = ofoldMap1 id
{-# INLINE ofold1 #-}


-- @'foldr1' f = 'Prelude.foldr1' f . 'otoList'@
ofoldr1 :: MonoFoldable mono
        => (Element mono -> Element mono -> Element mono)
        -> MinLen (Succ nat) mono
        -> Element mono
ofoldr1 f = ofoldr1Ex f . unMinLen
{-# INLINE ofoldr1 #-}

-- | A variant of 'ofoldl\'' that has no base case,
-- and thus may only be applied to non-empty structures.
--
-- @'foldl1\'' f = 'Prelude.foldl1' f . 'otoList'@
ofoldl1' :: MonoFoldable mono
         => (Element mono -> Element mono -> Element mono)
         -> MinLen (Succ nat) mono
         -> Element mono
ofoldl1' f = ofoldl1Ex' f . unMinLen
{-# INLINE ofoldl1' #-}

-- | like Data.List, but not partial on a MonoFoldable
maximum :: MonoFoldableOrd mono
        => MinLen (Succ nat) mono
        -> Element mono
maximum = maximumEx . unMinLen
{-# INLINE maximum #-}

-- | like Data.List, but not partial on a MonoFoldable
minimum :: MonoFoldableOrd mono
        => MinLen (Succ nat) mono
        -> Element mono
minimum = minimumEx . unMinLen
{-# INLINE minimum #-}

-- | like Data.List, but not partial on a MonoFoldable
maximumBy :: MonoFoldable mono
          => (Element mono -> Element mono -> Ordering)
          -> MinLen (Succ nat) mono
          -> Element mono
maximumBy cmp = maximumByEx cmp . unMinLen
{-# INLINE maximumBy #-}

-- | like Data.List, but not partial on a MonoFoldable
minimumBy :: MonoFoldable mono
          => (Element mono -> Element mono -> Ordering)
          -> MinLen (Succ nat) mono
          -> Element mono
minimumBy cmp = minimumByEx cmp . unMinLen
{-# INLINE minimumBy #-}
