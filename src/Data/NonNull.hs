{-# LANGUAGE TypeFamilies #-}
module Data.NonNull where

import Data.MonoTraversable
import Data.Sequences
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup

import qualified Data.Vector as V
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)



class IsSequence seq => Present seq where
    type NotNull seq
    nsingleton :: Element seq -> NotNull seq

    fromNonEmpty :: NonEmpty (Element seq) -> NotNull seq

    nfilter :: (Element seq -> Bool) -> NotNull seq -> seq


instance Present [a] where
    type NotNull [a] = NonEmpty a
    nsingleton = (NE.:| [])
    nfilter = NE.filter
    fromNonEmpty = id

data IsSequence seq => NonNull seq = NonNull { toSequence :: seq }

instance Present (Seq.Seq a) where
    type NotNull (Seq a) = NonNull (Seq a)
    nsingleton = NonNull . Seq.singleton
    fromNonEmpty = NonNull . Seq.fromList . NE.toList
    nfilter f = Seq.filter f . toSequence

instance Present (V.Vector a) where
    type NotNull (V.Vector a) = NonNull (V.Vector a)
    nsingleton = NonNull . V.singleton
    fromNonEmpty = NonNull . V.fromList . NE.toList
    nfilter f = V.filter f . toSequence

infixr 5 .|, <|
-- | a stream support efficient modification of the front of the sequence
class Present seq => Stream seq where
    -- | Prepend an element, creating a NotNull
    -- Data.List.NonEmpty gets to use the (:|) operator, which is nicer
    (.|) :: Element seq -> seq -> NotNull seq
    -- | Prepend an element to a NotNull
    (<|) :: Element seq -> NotNull seq -> NotNull seq

instance Stream [a] where
    (.|) = (NE.:|)
    (<|) = (NE.<|)

instance Stream (Seq a) where
    (.|) x = NonNull . (x Seq.<|)
    (<|) x = NonNull . (x Seq.<|) . toSequence

