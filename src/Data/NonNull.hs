{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
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
  , SafeSequence(..)
  , NotEmpty
  , (<|)
) where

import Prelude hiding (head, tail, init, last, reverse, seq, filter, replicate)
import Data.MonoTraversable
import Data.Sequences
import Control.Exception.Base (Exception, throw)
import Data.Monoid (mempty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Foldable as F

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as VS
import qualified Data.Sequence as Seq
import Data.Data

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
class SemiSequence seq => NonNull seq where
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

    -- | like 'Sequence.filter', but starts with a NonNull
    nfilter :: (Element seq -> Bool) -> seq -> Nullable seq

    -- | like 'Sequence.filterM', but starts with a NonNull
    nfilterM :: Monad m => (Element seq -> m Bool) -> seq -> m (Nullable seq)

    -- | i must be > 0. like 'Sequence.replicate'
    nReplicate :: Index seq -> Element seq -> seq


-- | SafeSequence contains functions that would be partial on a 'Nullable'
class SafeSequence seq where
    -- | like Data.List, but not partial on a NonEmpty
    head :: seq -> Element seq
    -- | like Data.List, but not partial on a NonEmpty
    tail :: seq -> Nullable seq
    -- | like Data.List, but not partial on a NonEmpty
    last :: seq -> Element seq
    -- | like Data.List, but not partial on a NonEmpty
    init :: seq -> Nullable seq




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

instance SafeSequence (NE.NonEmpty a) where
    head = NE.head
    tail = NE.tail
    last = NE.last
    init = NE.init


-- | a newtype wrapper indicating there are 1 or more elements
-- unwrap with 'toNullable'
newtype NotEmpty seq = NotEmpty { fromNotEmpty :: seq }
                       deriving (Eq, Ord, Read, Show, Data, Typeable, Functor)

type instance Element (NotEmpty seq) = Element seq
instance MonoFunctor seq => MonoFunctor (NotEmpty seq) where
  omap f (NotEmpty xs) = NotEmpty $ omap f xs

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
instance (IsSequence seq) => NonNull (NotEmpty seq) where
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


instance SafeSequence (NotEmpty (Seq.Seq a)) where
    head = flip Seq.index 1 . fromNotEmpty
    last (NotEmpty xs) = Seq.index xs (Seq.length xs - 1)
    tail = Seq.drop 1 . fromNotEmpty
    init (NotEmpty xs) = Seq.take (Seq.length xs - 1) xs

instance SafeSequence (NotEmpty (V.Vector a)) where
    head = V.head . fromNotEmpty
    tail = V.tail . fromNotEmpty
    last = V.last . fromNotEmpty
    init = V.init . fromNotEmpty

instance U.Unbox a => SafeSequence (NotEmpty (U.Vector a)) where
    head = U.head . fromNotEmpty
    tail = U.tail . fromNotEmpty
    last = U.last . fromNotEmpty
    init = U.init . fromNotEmpty

instance VS.Storable a => SafeSequence (NotEmpty (VS.Vector a)) where
    head = VS.head . fromNotEmpty
    tail = VS.tail . fromNotEmpty
    last = VS.last . fromNotEmpty
    init = VS.init . fromNotEmpty

instance SafeSequence (NotEmpty S.ByteString) where
    head = S.head . fromNotEmpty
    tail = S.tail . fromNotEmpty
    last = S.last . fromNotEmpty
    init = S.init . fromNotEmpty

instance SafeSequence (NotEmpty T.Text) where
    head = T.head . fromNotEmpty
    tail = T.tail . fromNotEmpty
    last = T.last . fromNotEmpty
    init = T.init . fromNotEmpty

instance SafeSequence (NotEmpty L.ByteString) where
    head = L.head . fromNotEmpty
    tail = L.tail . fromNotEmpty
    last = L.last . fromNotEmpty
    init = L.init . fromNotEmpty

instance SafeSequence (NotEmpty TL.Text) where
    head = TL.head . fromNotEmpty
    tail = TL.tail . fromNotEmpty
    last = TL.last . fromNotEmpty
    init = TL.init . fromNotEmpty

infixr 5 <|

-- | Prepend an element to a NonNull
(<|) :: NonNull seq => Element seq -> seq -> seq
(<|) = cons


class (NonNull seq, Ord (Element seq)) => OrdNonNull seq where
    -- | like Data.List, but not partial on a NonNull
    maximum :: seq -> Element seq
    default maximum :: (F.Foldable t, t a ~ Nullable seq, a ~ Element (t a), a ~ Element seq) => seq -> Element seq
    maximum = F.maximum . toNullable

    -- | like Data.List, but not partial on a NonNull
    minimum :: seq -> Element seq
    default minimum :: (F.Foldable t, t a ~ Nullable seq, a ~ Element (t a), a ~ Element seq) => seq -> Element seq
    minimum = F.minimum . toNullable

    -- | like Data.List, but not partial on a NonNull
    maximumBy :: (Element seq -> Element seq -> Ordering) -> seq -> Element seq
    default maximumBy :: (F.Foldable t, t a ~ Nullable seq, a ~ Element (t a), a ~ Element seq) => (Element seq -> Element seq -> Ordering) -> seq -> Element seq
    maximumBy f = F.maximumBy f . toNullable

    -- | like Data.List, but not partial on a NonNull
    minimumBy :: (Element seq -> Element seq -> Ordering) -> seq -> Element seq
    default minimumBy :: (F.Foldable t, t a ~ Nullable seq, a ~ Element (t a), a ~ Element seq) => (Element seq -> Element seq -> Ordering) -> seq -> Element seq
    minimumBy f = F.minimumBy f . toNullable

instance Ord a => OrdNonNull (NE.NonEmpty a) where
    maximum = F.maximum
    minimum = F.minimum
    maximumBy = F.maximumBy
    minimumBy = F.minimumBy

instance Ord a => OrdNonNull (NotEmpty (Seq.Seq a))
instance Ord a => OrdNonNull (NotEmpty (V.Vector a))

-- Problem, these are MonoFoldable, not Foldable
-- try replacing default Foldable constraint with MonoFoldable, need to re-implement Foldable methods
-- instance OrdNonNull (NotEmpty (S.ByteString))
-- instance OrdNonNull (NotEmpty (L.ByteString))
-- instance OrdNonNull (NotEmpty (T.Text))
-- instance OrdNonNull (NotEmpty (TL.Text))
