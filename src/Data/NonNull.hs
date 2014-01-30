{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
-- | Warning, this is Experimental!
--
-- Data.NonNull attempts to extend the concepts from
-- 'Data.List.NonEmpty' to any 'IsSequence'.
--
-- 'NonNull' is for a sequence with 1 or more elements.
-- 'Stream' is for a 'NonNull' that supports efficient
-- modification of the front of the sequence.
--
-- This code is experimental and likely to change dramatically and future versions.
-- Please send your feedback.
module Data.NonNull (
    NonNull(..)
  , head
  , tail
  , last
  , init
  , NotEmpty
  , asNotEmpty
  , MonoFoldable1(..)
  , maximum
  , maximumBy
  , minimum
  , minimumBy
  , (<|)
) where

import Prelude hiding (head, tail, init, last, reverse, seq, filter, replicate, maximum, minimum)
import Data.MonoTraversable
import Data.Sequences
import Control.Exception.Base (Exception, throw)
import Data.Semigroup
import qualified Data.Monoid as Monoid
import Data.Data
import Data.Maybe (fromMaybe)
import qualified Data.List.NonEmpty as NE

data NullError = NullError String deriving (Show, Typeable)
instance Exception NullError

-- | a 'NonNull' sequence has 1 or more items
-- In contrast, 'IsSequence' is allowed to have zero items.
--
-- Any NonNull functions that
-- decreases the number of elements in the sequences
-- will return a different 'Nullable' type.
--
-- The Nullable type for a 'NonEmpty' List is the normal List '[]'
--
-- NonNull allows one to safely perform what would otherwise be partial functions.
-- Hopefully you have abandoned partial functions, perhaps you are using the safe package.
-- However, safe essentially provides convenience functions for null checking.
-- With NonNull rather than always reacting with null checks we can proactively encode in our program when we know that a type is NonNull.
-- Now we have an invariant encoded in our types, making our program easier to understand.
-- This information is leveraged to avoid awkward null checking later on.
class (SemiSequence seq, IsSequence (Nullable seq), Element seq ~ Element (Nullable seq)) => NonNull seq where
    type Nullable seq

    -- | safely construct a 'NonNull' sequence from a 'NonEmpty' list
    fromNonEmpty :: NE.NonEmpty (Element seq) -> seq

    -- | safely convert a 'Nullable' to a 'NonNull'
    fromNullable :: Nullable seq -> Maybe seq

    -- | convert a 'Nullable' with elements to a 'NonNull'
    -- throw an exception if the 'Nullable' is empty.
    -- do not use this unless you have proved your structure is non-null
    nonNull :: Nullable seq -> seq
    nonNull nullable = case fromNullable nullable of
                         Nothing -> throw $ NullError "Data.NonNull.nonNull (NonNull default): expected non-null"
                         Just xs -> xs

    -- | used internally to construct a 'NonNull'.
    -- does not check whether the 'Nullable' is empty
    -- do not use this unless you have proved your structure is nonNull
    -- nonNullUnsafe :: Nullable seq -> seq

    -- | convert a 'NonNull' to a 'Nullable'
    toNullable :: seq -> Nullable seq

    -- | Like cons, prepends an element.
    -- However, the prepend is to a Nullable, creating a 'NonNull'
    --
    -- Generally this uses cons underneath.
    -- cons is not efficient for most data structures.
    --
    -- Alternatives:
    --   * if you don't need to cons, use 'fromNullable' or 'nonNull' if you can create your structure in one go.
    --   * if you need to cons, you might be able to start off with an efficient data structure such as a 'NonEmpty' List.
    --     'fronNonEmpty' will convert that to your data structure using the structure's fromList function.
    ncons :: Element seq -> Nullable seq -> seq

    -- | like 'uncons' of 'SemiSequence'
    nuncons :: seq -> (Element seq, Maybe seq)
    nuncons xs = case uncons $ toNullable xs of
                   Nothing -> error "Data.NonNull.nuncons: data structure is null, it should be non-null"
                   Just (x, xsNullable) -> (x, fromNullable xsNullable)

    -- | like 'uncons' of 'SemiSequence'
    splitFirst :: seq -> (Element seq, Nullable seq)
    splitFirst xs = case uncons $ toNullable xs of
                     Nothing -> error "Data.NonNull.splitFirst: data structure is null, it should be non-null"
                     Just tup -> tup


    -- | like 'Sequence.filter', but starts with a NonNull
    nfilter :: (Element seq -> Bool) -> seq -> Nullable seq

    -- | like 'Sequence.filterM', but starts with a NonNull
    nfilterM :: Monad m => (Element seq -> m Bool) -> seq -> m (Nullable seq)

    -- | i must be > 0. like 'Sequence.replicate'
    nReplicate :: Index seq -> Element seq -> seq

{-
maybeToNullable :: (Monoid (Nullable seq), NonNull seq) => Maybe seq -> Nullable seq
maybeToNullable Nothing   = mempty
maybeToNullable (Just xs) = toNullable xs
-}

-- | like Data.List, but not partial on a NonEmpty
head :: NonNull seq => seq -> Element seq
head = partialHead . toNullable
{-# INLINE head #-}

-- | like Data.List, but not partial on a NonEmpty
tail :: NonNull seq => seq -> Nullable seq
tail = partialTail . toNullable
{-# INLINE tail #-}

-- | like Data.List, but not partial on a NonEmpty
last :: NonNull seq => seq -> Element seq
last = partialLast . toNullable
{-# INLINE last #-}

-- | like Data.List, but not partial on a NonEmpty
init :: NonNull seq => seq -> Nullable seq
init = partialInit . toNullable
{-# INLINE init #-}



-- | NonNull list reuses 'Data.List.NonEmpty'
instance NonNull (NE.NonEmpty a) where
    type Nullable (NE.NonEmpty a) = [a]

    fromNonEmpty = id
    {-# INLINE fromNonEmpty #-}
    fromNullable = NE.nonEmpty

    nonNull = NE.fromList
    -- nonNullUnsafe = nonNull

    toNullable = NE.toList

    ncons = (NE.:|)

    nfilter = NE.filter
    nfilterM f = filterM f . toNullable

    nReplicate i x = NE.unfold unfold i
      where
        unfold countdown | countdown < 1 = (x, Nothing)
                         | otherwise     = (x, Just (countdown - 1))


-- | a newtype wrapper indicating there are 1 or more elements
-- unwrap with 'toNullable'
newtype NotEmpty seq = NotEmpty { fromNotEmpty :: seq }
                       deriving (Eq, Ord, Read, Show, Data, Typeable, Functor)
type instance Element (NotEmpty seq) = Element seq
deriving instance MonoFunctor seq => MonoFunctor (NotEmpty seq)
deriving instance MonoFoldable seq => MonoFoldable (NotEmpty seq)
deriving instance MonoTraversable seq => MonoTraversable (NotEmpty seq)

-- | Helper functions for type inferences.
--
-- Since 0.3.0
asNotEmpty :: NotEmpty a -> NotEmpty a
asNotEmpty = id
{-# INLINE asNotEmpty #-}

instance Monoid seq => Semigroup (NotEmpty seq) where
  x <> y  = NotEmpty (fromNotEmpty x `Monoid.mappend` fromNotEmpty y)
  sconcat = NotEmpty . Monoid.mconcat . fmap fromNotEmpty . NE.toList



instance SemiSequence seq => SemiSequence (NotEmpty seq) where
    type Index (NotEmpty seq) = Index seq

    singleton     = NotEmpty . singleton
    intersperse e = fmap $ intersperse e
    reverse       = fmap reverse
    find f        = find f . fromNotEmpty
    cons x        = fmap $ cons x
    snoc xs x     = fmap (flip snoc x) xs
    sortBy f      = fmap $ sortBy f


-- normally we favor defaulting, should we use it here?
-- this re-uses IsSequence functions and IsSequence uses defaulting
instance IsSequence seq => NonNull (NotEmpty seq) where
    type Nullable (NotEmpty seq) = seq

    fromNonEmpty = NotEmpty . fromList . NE.toList
    fromNullable xs | onull xs = Nothing
                    | otherwise = Just $ NotEmpty xs

    nonNull xs | onull xs = throw $ NullError "Data.NonNull.nonNull expected NotEmpty"
               | otherwise = NotEmpty xs

    -- nonNullUnsafe = NotEmpty
    toNullable = fromNotEmpty
    ncons x xs = NotEmpty $ cons x xs

    -- | i must be > 0. like 'Sequence.replicate'
    -- < 0 produces a 1 element NonEmpty
    nReplicate i x | i < 1 = ncons x mempty
                   | otherwise = NotEmpty $ replicate i x

    nfilter f = filter f . toNullable
    nfilterM f = filterM f . toNullable

infixr 5 <|

-- | Prepend an element to a NonNull
(<|) :: NonNull seq => Element seq -> seq -> seq
(<|) = cons


-- | fold operations that assume one or more elements
-- Guaranteed to be safe on a NonNull
class (NonNull seq, MonoFoldable (Nullable seq)) => MonoFoldable1 seq where
  ofoldMap1 :: Semigroup m => (Element seq -> m) -> seq -> m
  ofoldMap1 f = maybe (error "Data.NonNull.foldMap1 (MonoFoldable1)") id . getOption . ofoldMap (Option . Just . f) . toNullable

  -- ofold1 :: (Semigroup m ~ Element seq) => seq -> Element seq
  -- ofold1 = ofoldMap1 id

  -- @'foldr1' f = 'Prelude.foldr1' f . 'otoList'@
  ofoldr1 :: (Element seq -> Element seq -> Element seq) -> seq -> Element seq
  ofoldr1 f = fromMaybe (error "Data.NonNull.foldr1 (MonoFoldable1): empty structure") .
                  (ofoldr mf Nothing) . toNullable
    where
      mf x Nothing = Just x
      mf x (Just y) = Just (f x y)

  -- | A variant of 'ofoldl\'' that has no base case,
  -- and thus may only be applied to non-empty structures.
  --
  -- @'foldl1\'' f = 'Prelude.foldl1' f . 'otoList'@
  ofoldl1' :: (Element seq -> Element seq -> Element seq) -> seq -> Element seq
  ofoldl1' f = fromMaybe (error "ofoldl1': empty structure") .
                  (ofoldl' mf Nothing) . toNullable
    where
      mf Nothing y = Just y
      mf (Just x) y = Just (f x y)


instance MonoFoldable1 (NE.NonEmpty a)
-- normally we favor defaulting, should we be using it here?
instance (MonoFoldable mono, IsSequence mono) => MonoFoldable1 (NotEmpty mono)


-- | like Data.List, but not partial on a NonNull
maximum :: (OrdSequence (Nullable seq), NonNull seq) => seq -> Element seq
maximum = partialMaximum . toNullable
{-# INLINE maximum #-}

-- | like Data.List, but not partial on a NonNull
minimum :: (OrdSequence (Nullable seq), NonNull seq) => seq -> Element seq
minimum = partialMinimum . toNullable
{-# INLINE minimum #-}

-- | like Data.List, but not partial on a NonNull
maximumBy :: (OrdSequence (Nullable seq), NonNull seq)
          => (Element seq -> Element seq -> Ordering) -> seq -> Element seq
maximumBy cmp = partialMaximumBy cmp . toNullable

-- | like Data.List, but not partial on a NonNull
minimumBy :: (OrdSequence (Nullable seq), NonNull seq)
          => (Element seq -> Element seq -> Ordering) -> seq -> Element seq
minimumBy cmp = partialMinimumBy cmp . toNullable
