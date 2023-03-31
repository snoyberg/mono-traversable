{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | "Data.NonNull" extends the concepts from
-- "Data.List.NonEmpty" to any 'MonoFoldable'.
--
-- 'NonNull' is a newtype wrapper for a container with 1 or more elements.
module Data.NonNull (
    NonNull
  , fromNullable
  , impureNonNull
  , nonNull
  , toNullable
  , fromNonEmpty
  , toNonEmpty
  , ncons
  , nuncons
  , splitFirst
  , nfilter
  , nfilterM
  , nReplicate
  , head
  , tail
  , last
  , init
  , ofoldMap1
  , ofold1
  , ofoldr1
  , ofoldl1'
  , maximum
  , maximumBy
  , minimum
  , minimumBy
  , (<|)
  , toMinList
  , mapNonNull
  , GrowingAppend
) where

import Prelude hiding (head, tail, init, last, reverse, seq, filter, replicate, maximum, minimum)
import Control.Arrow (second)
import Control.Exception.Base (Exception, throw)
import Data.Data
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.MonoTraversable
import Data.Sequences
import Control.Monad.Trans.State.Strict (evalState, state)

data NullError = NullError String deriving (Show, Typeable)
instance Exception NullError

-- | A monomorphic container that is not null.
newtype NonNull mono = NonNull
    { toNullable :: mono
    -- ^ __Safely__ convert from a non-null monomorphic container to a nullable monomorphic container.
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
type instance Element (NonNull mono) = Element mono
deriving instance MonoFunctor mono => MonoFunctor (NonNull mono)
deriving instance MonoFoldable mono => MonoFoldable (NonNull mono)
instance MonoTraversable mono => MonoTraversable (NonNull mono) where
    otraverse f (NonNull x) = fmap NonNull (otraverse f x)
    {-# INLINE otraverse #-}

instance GrowingAppend mono => GrowingAppend (NonNull mono)

instance (Semigroup mono, GrowingAppend mono) => Semigroup (NonNull mono) where
    NonNull x <> NonNull y = NonNull (x <> y)

instance SemiSequence seq => SemiSequence (NonNull seq) where
    type Index (NonNull seq) = Index seq

    intersperse e = unsafeMap $ intersperse e
    reverse       = unsafeMap reverse
    find f        = find f . toNullable
    cons x        = unsafeMap $ cons x
    snoc xs x     = unsafeMap (flip snoc x) xs
    sortBy f      = unsafeMap $ sortBy f

-- | This function is unsafe, and must not be exposed from this module.
unsafeMap :: (mono -> mono) -> NonNull mono -> NonNull mono
unsafeMap f (NonNull x) = NonNull (f x)

instance MonoPointed mono => MonoPointed (NonNull mono) where
    opoint = NonNull . opoint
    {-# INLINE opoint #-}
instance IsSequence mono => MonoComonad (NonNull mono) where
        oextract  = head
        oextend f (NonNull mono) = NonNull
                                 . flip evalState mono
                                 . ofor mono
                                 . const
                                 . state
                                 $ \mono' -> (f (NonNull mono'), tailEx mono')

-- | __Safely__ convert from an __unsafe__ monomorphic container to a __safe__
-- non-null monomorphic container.
fromNullable :: MonoFoldable mono => mono -> Maybe (NonNull mono)
fromNullable mono
    | onull mono = Nothing
    | otherwise = Just (NonNull mono)

-- | __Unsafely__ convert from an __unsafe__ monomorphic container to a __safe__
-- non-null monomorphic container.
--
-- Throws an exception if the monomorphic container is empty.
--
-- @since 1.0.0
impureNonNull :: MonoFoldable mono => mono -> NonNull mono
impureNonNull nullable =
  fromMaybe (throw $ NullError "Data.NonNull.impureNonNull (NonNull default): expected non-null")
          $ fromNullable nullable

-- | Old synonym for 'impureNonNull'
nonNull :: MonoFoldable mono => mono -> NonNull mono
nonNull = impureNonNull
{-# DEPRECATED nonNull "Please use the more explicit impureNonNull instead" #-}

-- | __Safely__ convert from a 'NonEmpty' list to a non-null monomorphic container.
fromNonEmpty :: IsSequence seq => NE.NonEmpty (Element seq) -> NonNull seq
fromNonEmpty = impureNonNull . fromList . NE.toList
{-# INLINE fromNonEmpty #-}

-- | __Safely__ convert from a 'NonNull' container to a 'NonEmpty' list.
--
-- @since 1.0.15.0
toNonEmpty :: MonoFoldable mono => NonNull mono -> NE.NonEmpty (Element mono)
toNonEmpty = NE.fromList . otoList

-- | Specializes 'fromNonEmpty' to lists only.
toMinList :: NE.NonEmpty a -> NonNull [a]
toMinList = fromNonEmpty

-- | Prepend an element to a 'SemiSequence', creating a non-null 'SemiSequence'.
--
-- Generally this uses cons underneath.
-- cons is not efficient for most data structures.
--
-- Alternatives:
--
-- * if you don't need to cons, use 'fromNullable' or 'nonNull' if you can create your structure in one go.
-- * if you need to cons, you might be able to start off with an efficient data structure such as a 'NonEmpty' List.
--     'fromNonEmpty' will convert that to your data structure using the structure's fromList function.
ncons :: SemiSequence seq => Element seq -> seq -> NonNull seq
ncons x xs = nonNull $ cons x xs

-- | Extract the first element of a sequence and the rest of the non-null sequence if it exists.
nuncons :: IsSequence seq => NonNull seq -> (Element seq, Maybe (NonNull seq))
nuncons xs =
  second fromNullable
    $ fromMaybe (error "Data.NonNull.nuncons: data structure is null, it should be non-null")
              $ uncons (toNullable xs)

-- | Same as 'nuncons' with no guarantee that the rest of the sequence is non-null.
splitFirst :: IsSequence seq => NonNull seq -> (Element seq, seq)
splitFirst xs =
  fromMaybe (error "Data.NonNull.splitFirst: data structure is null, it should be non-null")
          $ uncons (toNullable xs)

-- | Equivalent to @"Data.Sequences".'Data.Sequences.filter'@,
-- but works on non-nullable sequences.
nfilter :: IsSequence seq => (Element seq -> Bool) -> NonNull seq -> seq
nfilter f = filter f . toNullable

-- | Equivalent to @"Data.Sequences".'Data.Sequences.filterM'@,
-- but works on non-nullable sequences.
nfilterM :: (Monad m, IsSequence seq) => (Element seq -> m Bool) -> NonNull seq -> m seq
nfilterM f = filterM f . toNullable

-- | Equivalent to @"Data.Sequences".'Data.Sequences.replicate'@
--
-- @i@ must be @> 0@
--
-- @i <= 0@ is treated the same as providing @1@
nReplicate :: IsSequence seq => Index seq -> Element seq -> NonNull seq
nReplicate i = nonNull . replicate (max 1 i)

-- | __Safe__ version of 'tailEx', only working on non-nullable sequences.
tail :: IsSequence seq => NonNull seq -> seq
tail = tailEx . toNullable
{-# INLINE tail #-}

-- | __Safe__ version of 'initEx', only working on non-nullable sequences.
init :: IsSequence seq => NonNull seq -> seq
init = initEx . toNullable
{-# INLINE init #-}

infixr 5 <|

-- | Prepend an element to a non-null 'SemiSequence'.
(<|) :: SemiSequence seq => Element seq -> NonNull seq -> NonNull seq
x <| y = ncons x (toNullable y)

-- | Return the first element of a monomorphic container.
--
-- Safe version of 'headEx', only works on monomorphic containers wrapped in a
-- 'NonNull'.
head :: MonoFoldable mono => NonNull mono -> Element mono
head = headEx . toNullable
{-# INLINE head #-}

-- | Return the last element of a monomorphic container.
--
-- Safe version of 'lastEx', only works on monomorphic containers wrapped in a
-- 'NonNull'.
last :: MonoFoldable mono => NonNull mono -> Element mono
last = lastEx . toNullable
{-# INLINE last #-}

-- | Map each element of a monomorphic container to a semigroup, and combine the
-- results.
--
-- Safe version of 'ofoldMap1Ex', only works on monomorphic containers wrapped in a
-- 'NonNull'.
--
-- ==== __Examples__
--
-- @
-- > let xs = ncons ("hello", 1 :: 'Integer') [(" world", 2)]
-- > 'ofoldMap1' 'fst' xs
-- "hello world"
-- @
ofoldMap1 :: (MonoFoldable mono, Semigroup m) => (Element mono -> m) -> NonNull mono -> m
ofoldMap1 f = ofoldMap1Ex f . toNullable
{-# INLINE ofoldMap1 #-}

-- | Join a monomorphic container, whose elements are 'Semigroup's, together.
--
-- Safe, only works on monomorphic containers wrapped in a 'NonNull'.
--
-- ==== __Examples__
--
-- @
-- > let xs = ncons "a" ["b", "c"]
-- > xs
-- 'NonNull' {toNullable = ["a","b","c"]}
--
-- > 'ofold1' xs
-- "abc"
-- @
ofold1 :: (MonoFoldable mono, Semigroup (Element mono)) => NonNull mono -> Element mono
ofold1 = ofoldMap1 id
{-# INLINE ofold1 #-}

-- | Right-associative fold of a monomorphic container with no base element.
--
-- Safe version of 'ofoldr1Ex', only works on monomorphic containers wrapped in a
-- 'NonNull'.
--
-- @'foldr1' f = "Prelude".'Prelude.foldr1' f . 'otoList'@
--
-- ==== __Examples__
--
-- @
-- > let xs = ncons "a" ["b", "c"]
-- > 'ofoldr1' (++) xs
-- "abc"
-- @
ofoldr1 :: MonoFoldable mono
        => (Element mono -> Element mono -> Element mono)
        -> NonNull mono
        -> Element mono
ofoldr1 f = ofoldr1Ex f . toNullable
{-# INLINE ofoldr1 #-}

-- | Strict left-associative fold of a monomorphic container with no base
-- element.
--
-- Safe version of 'ofoldl1Ex'', only works on monomorphic containers wrapped in a
-- 'NonNull'.
--
-- @'foldl1'' f = "Prelude".'Prelude.foldl1'' f . 'otoList'@
--
-- ==== __Examples__
--
-- @
-- > let xs = ncons "a" ["b", "c"]
-- > 'ofoldl1'' (++) xs
-- "abc"
-- @
ofoldl1' :: MonoFoldable mono
         => (Element mono -> Element mono -> Element mono)
         -> NonNull mono
         -> Element mono
ofoldl1' f = ofoldl1Ex' f . toNullable
{-# INLINE ofoldl1' #-}

-- | Get the maximum element of a monomorphic container.
--
-- Safe version of 'maximumEx', only works on monomorphic containers wrapped in a
-- 'NonNull'.
--
-- ==== __Examples__
--
-- @
-- > let xs = ncons 1 [2, 3 :: Int]
-- > 'maximum' xs
-- 3
-- @
maximum :: (MonoFoldable mono, Ord (Element mono))
        => NonNull mono
        -> Element mono
maximum = maximumEx . toNullable
{-# INLINE maximum #-}

-- | Get the minimum element of a monomorphic container.
--
-- Safe version of 'minimumEx', only works on monomorphic containers wrapped in a
-- 'NonNull'.
--
-- ==== __Examples__
--
-- @
-- > let xs = ncons 1 [2, 3 :: Int]
-- > 'minimum' xs
-- 1
-- @
minimum :: (MonoFoldable mono, Ord (Element mono))
        => NonNull mono
        -> Element mono
minimum = minimumEx . toNullable
{-# INLINE minimum #-}

-- | Get the maximum element of a monomorphic container,
-- using a supplied element ordering function.
--
-- Safe version of 'maximumByEx', only works on monomorphic containers wrapped in a
-- 'NonNull'.
maximumBy :: MonoFoldable mono
          => (Element mono -> Element mono -> Ordering)
          -> NonNull mono
          -> Element mono
maximumBy cmp = maximumByEx cmp . toNullable
{-# INLINE maximumBy #-}

-- | Get the minimum element of a monomorphic container,
-- using a supplied element ordering function.
--
-- Safe version of 'minimumByEx', only works on monomorphic containers wrapped in a
-- 'NonNull'.
minimumBy :: MonoFoldable mono
          => (Element mono -> Element mono -> Ordering)
          -> NonNull mono
          -> Element mono
minimumBy cmp = minimumByEx cmp . toNullable
{-# INLINE minimumBy #-}

-- | 'fmap' over the underlying container in a 'NonNull'.
--
-- @since 1.0.6.0

-- ==== __Examples__
--
-- @
-- > let xs = 'ncons' 1 [2, 3 :: Int]
-- > 'mapNonNull' 'show' xs
-- 'NonNull' {toNullable = [\"1\",\"2\",\"3\"]}
-- @
mapNonNull :: (Functor f, MonoFoldable (f b))
           => (a -> b)
           -> NonNull (f a)
           -> NonNull (f b)
mapNonNull f = impureNonNull . fmap f . toNullable
