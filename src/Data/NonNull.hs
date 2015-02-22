{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
-- | Warning, this is Experimental!
--
-- "Data.NonNull" attempts to extend the concepts from
-- "Data.List.NonEmpty" to any 'MonoFoldable'.
--
-- 'NonNull' is a typeclass for a container with 1 or more elements.
-- "Data.List.NonEmpty" and 'NotEmpty a' are members of the typeclass
module Data.NonNull (
    NonNull
  , fromNullable
  , nonNull
  , toNullable
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
) where

import Prelude hiding (head, tail, init, last, reverse, seq, filter, replicate, maximum, minimum)
import Control.Arrow (second)
import Control.Exception.Base (Exception, throw)
import Data.Data
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.MinLen
import Data.MonoTraversable
import Data.Sequences

data NullError = NullError String deriving (Show, Typeable)
instance Exception NullError

-- | A monomorphic container that is not null.
type NonNull mono = MinLen (Succ Zero) mono

-- | __Safely__ convert from an __unsafe__ monomorphic container to a __safe__
-- non-null monomorphic container.
fromNullable :: MonoFoldable mono => mono -> Maybe (NonNull mono)
fromNullable = toMinLen

-- | __Unsafely__ convert from an __unsafe__ monomorphic container to a __safe__
-- non-null monomorphic container.
--
-- Throws an exception if the monomorphic container is empty.
nonNull :: MonoFoldable mono => mono -> NonNull mono
nonNull nullable =
  fromMaybe (throw $ NullError "Data.NonNull.nonNull (NonNull default): expected non-null")
          $ fromNullable nullable

-- | __Safely__ convert from a non-null monomorphic container to a nullable monomorphic container.
toNullable :: NonNull mono -> mono
toNullable = unMinLen

-- | __Safely__ convert from a 'NonEmpty' list to a non-null monomorphic container.
fromNonEmpty :: IsSequence seq => NE.NonEmpty (Element seq) -> NonNull seq
fromNonEmpty = nonNull . fromList . NE.toList
{-# INLINE fromNonEmpty #-}

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
--     'fronNonEmpty' will convert that to your data structure using the structure's fromList function.
ncons :: SemiSequence seq => Element seq -> seq -> NonNull seq
ncons x xs = nonNull $ cons x xs

-- | Extract the first element of a sequnce and the rest of the non-null sequence if it exists.
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

-- | Equivalent to @"Data.Sequence".'Data.Sequence.filter'@,
-- but works on non-nullable sequences.
nfilter :: IsSequence seq => (Element seq -> Bool) -> NonNull seq -> seq
nfilter f = filter f . toNullable

-- | Equivalent to @"Data.Sequence".'Data.Sequence.filterM'@,
-- but works on non-nullable sequences.
nfilterM :: (Monad m, IsSequence seq) => (Element seq -> m Bool) -> NonNull seq -> m seq
nfilterM f = filterM f . toNullable

-- | Equivalent to @"Data.Sequence".'Data.Sequence.replicate'@
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
