{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
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
module Data.NonNull where

import Prelude hiding (head, tail, init, last, reverse)
import Data.MonoTraversable
import Data.Sequences
import qualified Data.List.NonEmpty as NE
import qualified Data.Foldable as Foldable

import qualified Data.Vector as V
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Data


-- | a 'NonNull' sequence has 1 or more items
-- Whereas 'IsSequence' is allowed to have zero items.
--
-- Unlike 'IsSequence', any functions that
-- return an empty sequence or decreases the number of elements
-- use a different 'Nullable' type.
--
-- The Nullable type for a 'NonEmpty' List is the normal List '[]'
--
-- NonNull allows us to safely perform what would otherwise be partial functions.
-- Hopefully you have abandoned partial functions and are using the safe package.
-- However, safe essentially provides convenience functions for null checking and one can still ends up using error (but with more unique messages).
--
-- With NonNull rather than always reacting with null checks we can proactively encode in our program that a type is NonNull.
-- It is easier to understand what is happening in our programs based on our type signatures since we have encoded more information in them.
class SemiSequence seq => NonNull seq where
    type Nullable seq

    fromNonEmpty :: NE.NonEmpty (Element seq) -> seq

    -- | like Data.List, but not partial on a NonEmpty
    head :: seq -> Element seq
    -- | like Data.List, but not partial on a NonEmpty
    tail :: seq -> Nullable seq
    -- | like Data.List, but not partial on a NonEmpty
    last :: seq -> Element seq
    -- | like Data.List, but not partial on a NonEmpty
    init :: seq -> Nullable seq

    -- | like 'Sequence.filter', but starts with a NonNull
    nfilter :: (Element seq -> Bool) -> seq -> Nullable seq

    -- | Prepend an element, creating a NonNull
    -- Data.List.NonEmpty gets to use the (:|) operator,
    -- but this can't because it is not a data constructor
    (.:) :: Element seq -> Nullable seq -> seq




-- | NonNull list reuses 'Data.List.NonEmpty'
instance NonNull (NE.NonEmpty a) where
    type Nullable (NE.NonEmpty a) = [a]

    fromNonEmpty = id
    head = NE.head
    tail = NE.tail
    last = NE.last
    init = NE.init
    nfilter = NE.filter
    (.:) = (NE.:|)


-- | a newtype wrapper indicating there are 1 or more elements
-- unwrap with 'toNullable'
newtype NotEmpty seq = NotEmpty { toNullable :: seq }
                      deriving (Eq, Ord, Read, Show, Data, Typeable, Functor)

type instance Element (NotEmpty seq) = Element seq
instance MonoFunctor seq => MonoFunctor (NotEmpty seq) where
  omap f (NotEmpty xs) = NotEmpty $ omap f xs


instance SemiSequence seq => SemiSequence (NotEmpty seq) where
    type Index (NotEmpty seq) = Index seq

    singleton     = NotEmpty . singleton
    intersperse e = fmap $ intersperse e
    reverse       = fmap reverse
    find f        = find f . toNullable
    cons x        = fmap $ cons x
    snoc xs x     = fmap (flip snoc x) xs
    sortBy f      = fmap $ sortBy f


instance NonNull (NotEmpty (Seq.Seq a)) where
    type Nullable (NotEmpty (Seq a)) = Seq a

    fromNonEmpty = NotEmpty . Seq.fromList . NE.toList
    nfilter f = Seq.filter f . toNullable
    head = flip Seq.index 1 . toNullable
    last (NotEmpty xs) = Seq.index xs (Seq.length xs - 1)
    tail = Seq.drop 1 . toNullable
    init (NotEmpty xs) = Seq.take (Seq.length xs - 1) xs
    (.:) x = NotEmpty . cons x

instance NonNull (NotEmpty (V.Vector a)) where
    type Nullable (NotEmpty (V.Vector a)) = V.Vector a
    fromNonEmpty = NotEmpty . V.fromList . NE.toList
    nfilter f = V.filter f . toNullable
    head = V.head . toNullable
    tail = V.tail . toNullable
    last = V.last . toNullable
    init = V.init . toNullable
    (.:) x = NotEmpty . cons x

infixr 5 .:, <|

-- | Prepend an element to a NonNull
(<|) :: NonNull seq => Element seq -> seq -> seq
(<|) = cons


class (NonNull seq, Ord (Element seq)) => OrdNonNull seq where
    -- | like Data.List, but not partial on a NonNull
    maximum :: seq -> Element seq
    -- | like Data.List, but not partial on a NonNull
    minimum :: seq -> Element seq
    -- | like Data.List, but not partial on a NonNull
    maximumBy :: (Element seq -> Element seq -> Ordering) -> seq -> Element seq
    -- | like Data.List, but not partial on a NonNull
    minimumBy :: (Element seq -> Element seq -> Ordering) -> seq -> Element seq

instance Ord a => OrdNonNull (NE.NonEmpty a) where
    maximum = Foldable.maximum
    minimum = Foldable.minimum
    maximumBy = Foldable.maximumBy
    minimumBy = Foldable.minimumBy

instance Ord a => OrdNonNull (NotEmpty (Seq a)) where
    maximum = Foldable.maximum . toNullable
    minimum = Foldable.minimum . toNullable
    maximumBy f = Foldable.maximumBy f . toNullable
    minimumBy f = Foldable.minimumBy f . toNullable

instance Ord a => OrdNonNull (NotEmpty (V.Vector a)) where
    maximum = Foldable.maximum . toNullable
    minimum = Foldable.minimum . toNullable
    maximumBy f = Foldable.maximumBy f . toNullable
    minimumBy f = Foldable.minimumBy f . toNullable
