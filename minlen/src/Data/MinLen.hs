{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.MinLen
    ( -- * Type level naturals
      -- ** Peano numbers
      -- $peanoNumbers
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

import Prelude (Num (..), Maybe (..), Int, Ordering (..), Eq, Ord (..), Read, Show, Functor (..), ($), flip, const)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Control.Category
import Data.MonoTraversable
import Data.Sequences
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
#if !MIN_VERSION_base(4,8,0)
import Control.Monad (liftM)
#endif
import Control.Monad.Trans.State.Strict (evalState, state)

-- $peanoNumbers
-- <https://wiki.haskell.org/Peano_numbers Peano numbers> are a simple way to
-- represent natural numbers (0, 1, 2...) using only a 'Zero' value and a
-- successor function ('Succ'). Each application of 'Succ' increases the number
-- by 1, so @Succ Zero@ is 1, @Succ (Succ Zero)@ is 2, etc.

-- | 'Zero' is the base value for the Peano numbers.
data Zero = Zero

-- | 'Succ' represents the next number in the sequence of natural numbers.
--
-- It takes a @nat@ (a natural number) as an argument.
--
-- 'Zero' is a @nat@, allowing @'Succ' 'Zero'@ to represent 1.
--
-- 'Succ' is also a @nat@, so it can be applied to itself, allowing
-- @'Succ' ('Succ' 'Zero')@ to represent 2,
-- @'Succ' ('Succ' ('Succ' 'Zero'))@ to represent 3, and so on.
data Succ nat = Succ nat

-- | Type-level natural number utility typeclass
class TypeNat nat where
    -- | Turn a type-level natural number into a number
    --
    -- @
    -- > 'toValueNat' 'Zero'
    -- 0
    -- > 'toValueNat' ('Succ' ('Succ' ('Succ' 'Zero')))
    -- 3
    -- @
    toValueNat :: Num i => nat -> i

    -- | Get a data representation of a natural number type
    --
    -- @
    -- > 'typeNat' :: 'Succ' ('Succ' 'Zero')
    -- Succ (Succ Zero) -- Errors because Succ and Zero have no Show typeclass,
    --                  -- But this is what it would look like if it did.
    -- @
    typeNat :: nat

instance TypeNat Zero where
    toValueNat Zero = 0
    typeNat = Zero
instance TypeNat nat => TypeNat (Succ nat) where
    toValueNat (Succ nat) = 1 + toValueNat nat
    typeNat = Succ typeNat

-- | Adds two type-level naturals.
--
-- See the 'mlappend' type signature for an example.
--
-- @
-- > :t 'typeNat' :: 'AddNat' ('Succ' ('Succ' 'Zero')) ('Succ' 'Zero')
--
-- 'typeNat' :: 'AddNat' ('Succ' ('Succ' 'Zero')) ('Succ' 'Zero')
--   :: 'Succ' ('Succ' ('Succ' 'Zero'))
-- @
type family AddNat x y
type instance AddNat Zero y = y
type instance AddNat (Succ x) y = AddNat x (Succ y)

-- | Calculates the maximum of two type-level naturals.
--
-- See the 'mlunion' type signature for an example.
--
-- @
-- > :t 'typeNat' :: 'MaxNat' ('Succ' ('Succ' 'Zero')) ('Succ' 'Zero')
--
-- 'typeNat' :: 'MaxNat' ('Succ' ('Succ' 'Zero')) ('Succ' 'Zero')
--   :: 'Succ' ('Succ' 'Zero')
-- @
type family MaxNat x y
type instance MaxNat Zero y = y
type instance MaxNat x Zero = x
type instance MaxNat (Succ x) (Succ y) = Succ (MaxNat x y)

-- | A wrapper around a container which encodes its minimum length in the type system.
-- This allows functions like 'head' and 'maximum' to be made safe without using 'Maybe'.
--
-- The length, @nat@, is encoded as a <https://wiki.haskell.org/Peano_numbers Peano number>,
-- which starts with the 'Zero' constructor and is made one larger with each application
-- of 'Succ' ('Zero' for 0, @'Succ' 'Zero'@ for 1, @'Succ' ('Succ' 'Zero')@ for 2, etc.).
-- Functions which require at least one element, then, are typed with @Succ nat@,
-- where @nat@ is either 'Zero' or any number of applications of 'Succ':
--
-- @
-- 'head' :: 'MonoTraversable' mono => 'MinLen' ('Succ' nat) mono -> 'Element' mono
-- @
--
-- The length is also a <https://wiki.haskell.org/Phantom_type phantom type>,
-- i.e. it is only used on the left hand side of the type and doesn't exist at runtime.
-- Notice how @'Succ' 'Zero'@ isn't included in the printed output:
--
-- @
-- > 'toMinLen' [1,2,3] :: 'Maybe' ('MinLen' ('Succ' 'Zero') ['Int'])
-- 'Just' ('MinLen' {unMinLen = [1,2,3]})
-- @
--
-- You can still use GHCI's @:i@ command to see the phantom type information:
--
-- @
-- > let xs = 'mlcons' 1 $ 'toMinLenZero' []
-- > :i xs
-- xs :: 'Num' t => 'MinLen' ('Succ' 'Zero') [t]
-- @
newtype MinLen nat mono =
    MinLen {
        unMinLen :: mono -- ^ Get the monomorphic container out of a 'MinLen' wrapper.
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

type instance Element (MinLen nat mono) = Element mono
deriving instance MonoFunctor mono => MonoFunctor (MinLen nat mono)
deriving instance MonoFoldable mono => MonoFoldable (MinLen nat mono)
instance MonoTraversable mono => MonoTraversable (MinLen nat mono) where
    otraverse f (MinLen x) = fmap MinLen (otraverse f x)
    {-# INLINE otraverse #-}
#if !MIN_VERSION_base(4,8,0)
    omapM f (MinLen x) = liftM MinLen (omapM f x)
    {-# INLINE omapM #-}
#endif
deriving instance GrowingAppend mono => GrowingAppend (MinLen nat mono)

-- | This function is unsafe, and must not be exposed from this module.
unsafeMap :: (mono -> mono) -> MinLen nat mono -> MinLen nat mono
unsafeMap f (MinLen x) = MinLen (f x)

instance (Semigroup mono, GrowingAppend mono) => Semigroup (MinLen nat mono) where
    MinLen x <> MinLen y = MinLen (x <> y)

instance SemiSequence seq => SemiSequence (MinLen nat seq) where
    type Index (MinLen nat seq) = Index seq

    intersperse e = unsafeMap $ intersperse e
    reverse       = unsafeMap reverse
    find f        = find f . unMinLen
    cons x        = unsafeMap $ cons x
    snoc xs x     = unsafeMap (flip snoc x) xs
    sortBy f      = unsafeMap $ sortBy f

instance MonoPointed mono => MonoPointed (MinLen Zero mono) where
    opoint = MinLen . opoint
    {-# INLINE opoint #-}
instance MonoPointed mono => MonoPointed (MinLen (Succ Zero) mono) where
    opoint = MinLen . opoint
    {-# INLINE opoint #-}

-- | Get the 'typeNat' of a 'MinLen' container.
natProxy :: TypeNat nat => MinLen nat mono -> nat
natProxy _ = typeNat

-- | Types a container as having a minimum length of zero. This is useful when combined with other 'MinLen'
-- functions that increase the size of the container.
--
-- ==== __Examples__
--
-- @
-- > 1 \`mlcons` 'toMinLenZero' []
-- 'MinLen' {unMinLen = [1]}
-- @
toMinLenZero :: (MonoFoldable mono) => mono -> MinLen Zero mono
toMinLenZero = MinLen

-- | Attempts to add a 'MinLen' constraint to a monomorphic container.
--
-- ==== __Examples__
--
-- @
-- > let xs = 'toMinLen' [1,2,3] :: 'Maybe' ('MinLen' ('Succ' 'Zero') ['Int'])
-- > xs
-- 'Just' ('MinLen' {unMinLen = [1,2,3]})
--
-- > :i xs
-- xs :: 'Maybe' ('MinLen' ('Succ' 'Zero') ['Int'])
-- @
--
-- @
-- > 'toMinLen' [] :: 'Maybe' ('MinLen' ('Succ' 'Zero') ['Int'])
-- 'Nothing'
-- @
toMinLen :: (MonoFoldable mono, TypeNat nat) => mono -> Maybe (MinLen nat mono)
toMinLen mono =
    case ocompareLength mono (toValueNat nat :: Int) of
        LT -> Nothing
        _  -> Just res'
  where
    nat = natProxy res'
    res' = MinLen mono

-- | __Unsafe__
--
-- Although this function itself cannot cause a segfault, it breaks the
-- safety guarantees of 'MinLen' and can lead to a segfault when using
-- otherwise safe functions.
--
-- ==== __Examples__
--
-- @
-- > let xs = 'unsafeToMinLen' [] :: 'MinLen' ('Succ' 'Zero') ['Int']
-- > 'olength' xs
-- 0
-- > 'head' xs
-- *** Exception: Data.MonoTraversable.headEx: empty
-- @
unsafeToMinLen :: mono -> MinLen nat mono
unsafeToMinLen = MinLen

infixr 5 `mlcons`

-- | Adds an element to the front of a list, increasing its minimum length by 1.
--
-- ==== __Examples__
--
-- @
-- > let xs = 'unsafeToMinLen' [1,2,3] :: 'MinLen' ('Succ' 'Zero') ['Int']
-- > 0 \`mlcons` xs
-- 'MinLen' {unMinLen = [0,1,2,3]}
-- @
mlcons :: IsSequence seq => Element seq -> MinLen nat seq -> MinLen (Succ nat) seq
mlcons e (MinLen seq) = MinLen (cons e seq)
{-# INLINE mlcons #-}

-- | Concatenate two sequences, adding their minimum lengths together.
--
-- ==== __Examples__
--
-- @
-- > let xs = 'unsafeToMinLen' [1,2,3] :: 'MinLen' ('Succ' 'Zero') ['Int']
-- > xs \`mlappend` xs
-- 'MinLen' {unMinLen = [1,2,3,1,2,3]}
-- @
mlappend :: IsSequence seq => MinLen x seq -> MinLen y seq -> MinLen (AddNat x y) seq
mlappend (MinLen x) (MinLen y) = MinLen (x `mappend` y)
{-# INLINE mlappend #-}

-- | Return the first element of a monomorphic container.
--
-- Safe version of 'headEx', only works on monomorphic containers wrapped in a
-- @'MinLen' ('Succ' nat)@.
head :: MonoFoldable mono => MinLen (Succ nat) mono -> Element mono
head = headEx . unMinLen
{-# INLINE head #-}

-- | Return the last element of a monomorphic container.
--
-- Safe version of 'lastEx', only works on monomorphic containers wrapped in a
-- @'MinLen' ('Succ' nat)@.
last :: MonoFoldable mono => MinLen (Succ nat) mono -> Element mono
last = lastEx . unMinLen
{-# INLINE last #-}

-- | Returns all but the first element of a sequence, reducing its 'MinLen' by 1.
--
-- Safe, only works on sequences wrapped in a @'MinLen' ('Succ' nat)@.
--
-- ==== __Examples__
--
-- @
-- > let xs = 'toMinLen' [1,2,3] :: 'Maybe' ('MinLen' ('Succ' 'Zero') ['Int'])
-- > 'fmap' 'tailML' xs
-- 'Just' ('MinLen' {unMinLen = [2,3]})
-- @
tailML :: IsSequence seq => MinLen (Succ nat) seq -> MinLen nat seq
tailML = MinLen . tailEx . unMinLen

-- | Returns all but the last element of a sequence, reducing its 'MinLen' by 1.
--
-- Safe, only works on sequences wrapped in a @'MinLen' ('Succ' nat)@.
--
-- ==== __Examples__
--
-- @
-- > let xs = 'toMinLen' [1,2,3] :: 'Maybe' ('MinLen' ('Succ' 'Zero') ['Int'])
-- > 'fmap' 'initML' xs
-- 'Just' ('MinLen' {unMinLen = [1,2]})
-- @
initML :: IsSequence seq => MinLen (Succ nat) seq -> MinLen nat seq
initML = MinLen . initEx . unMinLen

-- | Joins two semigroups, keeping the larger 'MinLen' of the two.
--
-- ==== __Examples__
--
-- @
-- > let xs = 'unsafeToMinLen' [1] :: 'MinLen' ('Succ' 'Zero') ['Int']
-- > let ys = xs \`mlunion` xs
-- > ys
-- 'MinLen' {unMinLen = [1,1]}
--
-- > :i ys
-- ys :: 'MinLen' ('Succ' 'Zero') ['Int']
-- @
mlunion :: (Semigroup mono, GrowingAppend mono) => MinLen x mono -> MinLen y mono -> MinLen (MaxNat x y) mono
mlunion (MinLen x) (MinLen y) = MinLen (x <> y)

-- | Map each element of a monomorphic container to a semigroup, and combine the
-- results.
--
-- Safe version of 'ofoldMap1Ex', only works on monomorphic containers wrapped in a
-- @'MinLen' ('Succ' nat)@.
--
-- ==== __Examples__
--
-- @
-- > let xs = ("hello", 1 :: 'Integer') \`mlcons` (" world", 2) \`mlcons` ('toMinLenZero' [])
-- > 'ofoldMap1' 'fst' xs
-- "hello world"
-- @
ofoldMap1 :: (MonoFoldable mono, Semigroup m) => (Element mono -> m) -> MinLen (Succ nat) mono -> m
ofoldMap1 f = ofoldMap1Ex f . unMinLen
{-# INLINE ofoldMap1 #-}

-- | Join a monomorphic container, whose elements are 'Semigroup's, together.
--
-- Safe, only works on monomorphic containers wrapped in a @'MinLen' ('Succ' nat)@.
--
-- ==== __Examples__
--
-- @
-- > let xs = "a" \`mlcons` "b" \`mlcons` "c" \`mlcons` ('toMinLenZero' [])
-- > xs
-- 'MinLen' {unMinLen = ["a","b","c"]}
--
-- > 'ofold1' xs
-- "abc"
-- @
ofold1 :: (MonoFoldable mono, Semigroup (Element mono)) => MinLen (Succ nat) mono -> Element mono
ofold1 = ofoldMap1 id
{-# INLINE ofold1 #-}

-- | Right-associative fold of a monomorphic container with no base element.
--
-- Safe version of 'ofoldr1Ex', only works on monomorphic containers wrapped in a
-- @'MinLen' ('Succ' nat)@.
--
-- @'foldr1' f = "Prelude".'Prelude.foldr1' f . 'otoList'@
--
-- ==== __Examples__
--
-- @
-- > let xs = "a" \`mlcons` "b" \`mlcons` "c" \`mlcons` ('toMinLenZero' [])
-- > 'ofoldr1' (++) xs
-- "abc"
-- @
ofoldr1 :: MonoFoldable mono
        => (Element mono -> Element mono -> Element mono)
        -> MinLen (Succ nat) mono
        -> Element mono
ofoldr1 f = ofoldr1Ex f . unMinLen
{-# INLINE ofoldr1 #-}

-- | Strict left-associative fold of a monomorphic container with no base
-- element.
--
-- Safe version of 'ofoldl1Ex'', only works on monomorphic containers wrapped in a
-- @'MinLen' ('Succ' nat)@.
--
-- @'foldl1'' f = "Prelude".'Prelude.foldl1'' f . 'otoList'@
--
-- ==== __Examples__
--
-- @
-- > let xs = "a" \`mlcons` "b" \`mlcons` "c" \`mlcons` ('toMinLenZero' [])
-- > 'ofoldl1'' (++) xs
-- "abc"
-- @
ofoldl1' :: MonoFoldable mono
         => (Element mono -> Element mono -> Element mono)
         -> MinLen (Succ nat) mono
         -> Element mono
ofoldl1' f = ofoldl1Ex' f . unMinLen
{-# INLINE ofoldl1' #-}

-- | Get the maximum element of a monomorphic container.
--
-- Safe version of 'maximumEx', only works on monomorphic containers wrapped in a
-- @'MinLen' ('Succ' nat)@.
--
-- ==== __Examples__
--
-- @
-- > let xs = 'toMinLen' [1,2,3] :: 'Maybe' ('MinLen' ('Succ' 'Zero') ['Int'])
-- > 'fmap' 'maximum' xs
-- 'Just' 3
-- @
maximum :: (MonoFoldable mono, Ord (Element mono))
        => MinLen (Succ nat) mono
        -> Element mono
maximum = maximumEx . unMinLen
{-# INLINE maximum #-}

-- | Get the minimum element of a monomorphic container.
--
-- Safe version of 'minimumEx', only works on monomorphic containers wrapped in a
-- @'MinLen' ('Succ' nat)@.
--
-- ==== __Examples__
--
-- @
-- > let xs = 'toMinLen' [1,2,3] :: 'Maybe' ('MinLen' ('Succ' 'Zero') ['Int'])
-- > 'fmap' 'minimum' xs
-- 'Just' 1
-- @
minimum :: (MonoFoldable mono, Ord (Element mono))
        => MinLen (Succ nat) mono
        -> Element mono
minimum = minimumEx . unMinLen
{-# INLINE minimum #-}

-- | Get the maximum element of a monomorphic container,
-- using a supplied element ordering function.
--
-- Safe version of 'maximumByEx', only works on monomorphic containers wrapped in a
-- @'MinLen' ('Succ' nat)@.
maximumBy :: MonoFoldable mono
          => (Element mono -> Element mono -> Ordering)
          -> MinLen (Succ nat) mono
          -> Element mono
maximumBy cmp = maximumByEx cmp . unMinLen
{-# INLINE maximumBy #-}

-- | Get the minimum element of a monomorphic container,
-- using a supplied element ordering function.
--
-- Safe version of 'minimumByEx', only works on monomorphic containers wrapped in a
-- @'MinLen' ('Succ' nat)@.
minimumBy :: MonoFoldable mono
          => (Element mono -> Element mono -> Ordering)
          -> MinLen (Succ nat) mono
          -> Element mono
minimumBy cmp = minimumByEx cmp . unMinLen
{-# INLINE minimumBy #-}

-- | 'oextract' is 'head'.
--
-- For @'oextend' f@, the new 'mono' is populated by applying @f@ to
-- successive 'tail's of the original 'mono'.
--
-- For example, for @'MinLen' ('Succ' 'Zero') ['Int']@, or
-- @'NonNull' ['Int']@:
--
-- @
-- 'oextend' f [1,2,3,4,5] = [ f [1, 2, 3, 4, 5]
--                           , f [2, 3, 4, 5]
--                           , f [3, 4, 5]
--                           , f [4, 5]
--                           , f [5]
--                           ]
-- @
--
-- Meant to be a direct analogy to the instance for 'NonEmpty' @a@.
--
instance IsSequence mono
    => MonoComonad (MinLen (Succ Zero) mono) where
        oextract  = head
        oextend f (MinLen mono) = MinLen
                                . flip evalState mono
                                . ofor mono
                                . const
                                . state
                                $ \mono' -> (f (MinLen mono'), tailEx mono')
