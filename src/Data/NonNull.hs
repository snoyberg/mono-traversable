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
  , fromNonEmpty
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
  , NotEmpty
  , asNotEmpty
  , ofoldMap1
  , ofold1
  , ofoldr1
  , ofoldl1'
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
import qualified Data.List.NonEmpty as NE

data NullError = NullError String deriving (Show, Typeable)
instance Exception NullError

-- | a 'NonNull' has 1 or more items
--
-- In contrast, 'MonoFoldable' is allowed to have zero items.
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
class (MonoFoldable mono, MonoFoldable (Nullable mono), Element mono ~ Element (Nullable mono)) => NonNull mono where
    type Nullable mono

    -- | safely convert a 'Nullable' to a 'NonNull'
    fromNullable :: Nullable mono -> Maybe mono

    -- | convert a 'Nullable' with elements to a 'NonNull'
    -- throw an exception if the 'Nullable' is empty.
    -- do not use this unless you have proved your structure is non-null
    nonNull :: Nullable mono -> mono
    nonNull nullable = case fromNullable nullable of
                         Nothing -> throw $ NullError "Data.NonNull.nonNull (NonNull default): expected non-null"
                         Just xs -> xs

    -- | used internally to construct a 'NonNull'.
    -- does not check whether the 'Nullable' is empty
    -- do not use this unless you have proved your structure is nonNull
    -- nonNullUnsafe :: Nullable seq -> seq

    -- | convert a 'NonNull' to a 'Nullable'
    toNullable :: mono -> Nullable mono

-- | safely construct a 'NonNull' from a 'NonEmpty' list
fromNonEmpty :: (NonNull seq, IsSequence (Nullable seq)) => NE.NonEmpty (Element seq) -> seq
fromNonEmpty = nonNull . fromList . NE.toList
{-# INLINE fromNonEmpty #-}

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
ncons :: (NonNull seq, SemiSequence (Nullable seq)) => Element seq -> Nullable seq -> seq
ncons x xs = nonNull $ cons x xs

-- | like 'uncons' of 'SemiSequence'
nuncons :: (NonNull seq, IsSequence (Nullable seq)) => seq -> (Element seq, Maybe seq)
nuncons xs = case uncons $ toNullable xs of
               Nothing -> error "Data.NonNull.nuncons: data structure is null, it should be non-null"
               Just (x, xsNullable) -> (x, fromNullable xsNullable)

-- | like 'uncons' of 'SemiSequence'
splitFirst :: (IsSequence (Nullable seq), NonNull seq) => seq -> (Element seq, Nullable seq)
splitFirst xs = case uncons $ toNullable xs of
                 Nothing -> error "Data.NonNull.splitFirst: data structure is null, it should be non-null"
                 Just tup -> tup


-- | like 'Sequence.filter', but starts with a NonNull
nfilter :: (NonNull seq, IsSequence (Nullable seq))
        => (Element seq -> Bool) -> seq -> Nullable seq
nfilter f = filter f . toNullable

-- | like 'Sequence.filterM', but starts with a NonNull
nfilterM :: (NonNull seq, Monad m, IsSequence (Nullable seq))
         => (Element seq -> m Bool) -> seq -> m (Nullable seq)
nfilterM f = filterM f . toNullable

-- | i must be > 0. like 'Sequence.replicate'
--
-- i <= 0 is treated the same as providing 1
nReplicate :: (NonNull seq, Num (Index (Nullable seq)), Ord (Index (Nullable seq)), IsSequence (Nullable seq))
           => Index (Nullable seq) -> Element seq -> seq
nReplicate i = nonNull . replicate (max 1 i)

{-
maybeToNullable :: (Monoid (Nullable seq), NonNull seq) => Maybe seq -> Nullable seq
maybeToNullable Nothing   = mempty
maybeToNullable (Just xs) = toNullable xs
-}

-- | like Data.List, but not partial on a NonEmpty
head :: (IsSequence (Nullable seq), NonNull seq) => seq -> Element seq
head = headEx . toNullable
{-# INLINE head #-}

-- | like Data.List, but not partial on a NonEmpty
tail :: (IsSequence (Nullable seq), NonNull seq) => seq -> Nullable seq
tail = tailEx . toNullable
{-# INLINE tail #-}

-- | like Data.List, but not partial on a NonEmpty
last :: (IsSequence (Nullable seq), NonNull seq) => seq -> Element seq
last = lastEx . toNullable
{-# INLINE last #-}

-- | like Data.List, but not partial on a NonEmpty
init :: (IsSequence (Nullable seq), NonNull seq) => seq -> Nullable seq
init = initEx . toNullable
{-# INLINE init #-}



-- | NonNull list reuses 'Data.List.NonEmpty'
instance NonNull (NE.NonEmpty a) where
    type Nullable (NE.NonEmpty a) = [a]

    fromNullable = NE.nonEmpty

    nonNull = NE.fromList
    -- nonNullUnsafe = nonNull

    toNullable = NE.toList


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
instance MonoFoldable seq => NonNull (NotEmpty seq) where
    type Nullable (NotEmpty seq) = seq

    fromNullable xs | onull xs = Nothing
                    | otherwise = Just $ NotEmpty xs

    nonNull xs | onull xs = throw $ NullError "Data.NonNull.nonNull expected NotEmpty"
               | otherwise = NotEmpty xs

    -- nonNullUnsafe = NotEmpty
    toNullable = fromNotEmpty

infixr 5 <|

-- | Prepend an element to a NonNull
(<|) :: (SemiSequence (Nullable seq), NonNull seq) => Element seq -> seq -> seq
x <| y = ncons x (toNullable y)


ofoldMap1 :: (NonNull seq, Semigroup m) => (Element seq -> m) -> seq -> m
ofoldMap1 f = ofoldMap1Ex f . toNullable
{-# INLINE ofoldMap1 #-}

ofold1 :: (NonNull seq, Semigroup (Element seq)) => seq -> Element seq
ofold1 = ofoldMap1 id
{-# INLINE ofold1 #-}

-- @'foldr1' f = 'Prelude.foldr1' f . 'otoList'@
ofoldr1 :: NonNull seq => (Element seq -> Element seq -> Element seq) -> seq -> Element seq
ofoldr1 f = ofoldr1Ex f . toNullable
{-# INLINE ofoldr1 #-}

-- | A variant of 'ofoldl\'' that has no base case,
-- and thus may only be applied to non-empty structures.
--
-- @'foldl1\'' f = 'Prelude.foldl1' f . 'otoList'@
ofoldl1' :: NonNull seq => (Element seq -> Element seq -> Element seq) -> seq -> Element seq
ofoldl1' f = ofoldl1Ex' f . toNullable
{-# INLINE ofoldl1' #-}

-- | like Data.List, but not partial on a NonNull
maximum :: (MonoFoldableOrd (Nullable seq), NonNull seq) => seq -> Element seq
maximum = maximumEx . toNullable
{-# INLINE maximum #-}

-- | like Data.List, but not partial on a NonNull
minimum :: (MonoFoldableOrd (Nullable seq), NonNull seq) => seq -> Element seq
minimum = minimumEx . toNullable
{-# INLINE minimum #-}

-- | like Data.List, but not partial on a NonNull
maximumBy :: (MonoFoldableOrd (Nullable seq), NonNull seq)
          => (Element seq -> Element seq -> Ordering) -> seq -> Element seq
maximumBy cmp = maximumByEx cmp . toNullable
{-# INLINE maximumBy #-}

-- | like Data.List, but not partial on a NonNull
minimumBy :: (MonoFoldableOrd (Nullable seq), NonNull seq)
          => (Element seq -> Element seq -> Ordering) -> seq -> Element seq
minimumBy cmp = minimumByEx cmp . toNullable
{-# INLINE minimumBy #-}
