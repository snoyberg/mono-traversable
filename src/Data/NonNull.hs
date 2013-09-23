{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
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

import Prelude hiding (head, tail, init, last)
import Data.MonoTraversable
import Data.Sequences
import qualified Data.List.NonEmpty as NE
import Data.Semigroup
import qualified Data.Foldable as Foldable

import qualified Data.Vector as V
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)



-- | a NonNull sequence has 1 or more items
class IsSequence seq => NonNull seq where
    type NonEmpty seq

    nsingleton :: Element seq -> NonEmpty seq

    fromNonEmpty :: NE.NonEmpty (Element seq) -> NonEmpty seq

    -- | like 'Sequence.filter', but starts with a NonNull
    nfilter :: (Element seq -> Bool) -> NonEmpty seq -> seq

    -- | like Data.List, but not partial on a NonEmpty
    head :: NonEmpty seq -> Element seq
    -- | like Data.List, but not partial on a NonEmpty
    tail :: NonEmpty seq -> seq
    -- | like Data.List, but not partial on a NonEmpty
    last :: NonEmpty seq -> Element seq
    -- | like Data.List, but not partial on a NonEmpty
    init :: NonEmpty seq -> seq


-- | NonNull list reuses 'Data.List.NonEmpty'
instance NonNull [a] where
    type NonEmpty [a] = NE.NonEmpty a
    nsingleton = (NE.:| [])
    fromNonEmpty = id
    nfilter = NE.filter
    head = NE.head
    tail = NE.tail
    last = NE.last
    init = NE.init


-- | a wrapper indicating there are 1 or more elements
-- unwrap with toSequence
data NotEmpty seq = NotEmpty { toSequence :: seq }

instance NonNull (Seq.Seq a) where
    type NonEmpty (Seq a) = NotEmpty (Seq a)
    nsingleton = NotEmpty . Seq.singleton
    fromNonEmpty = NotEmpty . Seq.fromList . NE.toList
    nfilter f = Seq.filter f . toSequence
    head = flip Seq.index 1 . toSequence
    last (NotEmpty seq) = Seq.index   seq (Seq.length seq - 1)
    tail = Seq.drop 1 . toSequence
    init (NotEmpty seq) = Seq.take (Seq.length seq - 1) seq

instance NonNull (V.Vector a) where
    type NonEmpty (V.Vector a) = NotEmpty (V.Vector a)
    nsingleton = NotEmpty . V.singleton
    fromNonEmpty = NotEmpty . V.fromList . NE.toList
    nfilter f = V.filter f . toSequence
    head = V.head . toSequence
    tail = V.tail . toSequence
    last = V.last . toSequence
    init = V.init . toSequence

infixr 5 .:, <|

-- | a stream is a NonNull that supports efficient modification of the front of the sequence
class NonNull seq => Stream seq where
    -- | Prepend an element, creating a NonEmpty
    -- Data.List.NonEmpty gets to use the (:|) operator,
    -- but this can't because it is not a data constructor
    (.:) :: Element seq -> seq -> NonEmpty seq
    -- | Prepend an element to a NonEmpty
    (<|) :: Element seq -> NonEmpty seq -> NonEmpty seq

instance Stream [a] where
    (.:) = (NE.:|)
    (<|) = (NE.<|)

instance Stream (Seq a) where
    (.:) x = NotEmpty . (x Seq.<|)
    (<|) x = NotEmpty . (x Seq.<|) . toSequence


{-
class (NonNull seq, Ord (Element seq)) => OrdNonNull seq where
    -- | like Data.List, but not partial on a NonEmpty
    maximum :: NonEmpty seq -> Element seq
    -- | like Data.List, but not partial on a NonEmpty
    minimum :: NonEmpty seq -> Element seq
    -- | like Data.List, but not partial on a NonEmpty
    maximumBy :: (Element seq -> Element seq -> Ordering) -> NonEmpty seq -> Element seq
    -- | like Data.List, but not partial on a NonEmpty
    minimumBy :: (Element seq -> Element seq -> Ordering) -> NonEmpty seq -> Element seq

instance Ord a => OrdNonNull [a] where
    maximum = Foldable.maximum
    minimum = Foldable.minimum
    maximumBy = Foldable.maximumBy
    minimumBy = Foldable.minimumBy

instance Ord a => OrdNonNull (Seq a) where
    maximum = Foldable.maximum
    minimum = Foldable.minimum
    maximumBy = Foldable.maximumBy
    minimumBy = Foldable.minimumBy

instance Ord a => OrdNonNull (V.Vector a) where
    maximum = Foldable.maximum
    minimum = Foldable.minimum
    maximumBy = Foldable.maximumBy
    minimumBy = Foldable.minimumBy
    -}
