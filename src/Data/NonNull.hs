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
import Data.MonoTraversable
import Data.Sequences
import Control.Exception.Base (Exception, throw)
import Data.Data
import qualified Data.List.NonEmpty as NE
import Data.MinLen

data NullError = NullError String deriving (Show, Typeable)
instance Exception NullError

type NonNull mono = MinLen (Succ Zero) mono

-- | safely convert a 'Nullable' to a 'NonNull'
fromNullable :: MonoFoldable mono => mono -> Maybe (NonNull mono)
fromNullable = toMinLen

-- | convert a 'Nullable' with elements to a 'NonNull'
-- throw an exception if the 'Nullable' is empty.
-- do not use this unless you have proved your structure is non-null
nonNull :: MonoFoldable mono => mono -> NonNull mono
nonNull nullable = case fromNullable nullable of
                     Nothing -> throw $ NullError "Data.NonNull.nonNull (NonNull default): expected non-null"
                     Just xs -> xs

-- | convert a 'NonNull' to a 'Nullable'
toNullable :: NonNull mono -> mono
toNullable = unMinLen

-- | safely construct a 'NonNull' from a 'NonEmpty' list
fromNonEmpty :: IsSequence seq => NE.NonEmpty (Element seq) -> NonNull seq
fromNonEmpty = nonNull . fromList . NE.toList
{-# INLINE fromNonEmpty #-}

toMinList :: NE.NonEmpty a -> NonNull [a] 
toMinList ne = fromNonEmpty ne

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
ncons :: SemiSequence seq => Element seq -> seq -> NonNull seq
ncons x xs = nonNull $ cons x xs

-- | like 'uncons' of 'SemiSequence'
nuncons :: IsSequence seq => NonNull seq -> (Element seq, Maybe (NonNull seq))
nuncons xs = case uncons $ toNullable xs of
               Nothing -> error "Data.NonNull.nuncons: data structure is null, it should be non-null"
               Just (x, xsNullable) -> (x, fromNullable xsNullable)

-- | like 'uncons' of 'SemiSequence'
splitFirst :: IsSequence seq => NonNull seq -> (Element seq, seq)
splitFirst xs = case uncons $ toNullable xs of
                 Nothing -> error "Data.NonNull.splitFirst: data structure is null, it should be non-null"
                 Just tup -> tup


-- | like 'Sequence.filter', but starts with a NonNull
nfilter :: IsSequence seq => (Element seq -> Bool) -> NonNull seq -> seq
nfilter f = filter f . toNullable

-- | like 'Sequence.filterM', but starts with a NonNull
nfilterM :: (Monad m, IsSequence seq) => (Element seq -> m Bool) -> NonNull seq -> m seq
nfilterM f = filterM f . toNullable

-- | i must be > 0. like 'Sequence.replicate'
--
-- i <= 0 is treated the same as providing 1
nReplicate :: IsSequence seq => Index seq -> Element seq -> NonNull seq
nReplicate i = nonNull . replicate (max 1 i)

-- | like Data.List, but not partial on a NonEmpty
tail :: IsSequence seq => NonNull seq -> seq
tail = tailEx . toNullable
{-# INLINE tail #-}

-- | like Data.List, but not partial on a NonEmpty
init :: IsSequence seq => NonNull seq -> seq
init = initEx . toNullable
{-# INLINE init #-}

infixr 5 <|

-- | Prepend an element to a NonNull
(<|) :: SemiSequence seq => Element seq -> NonNull seq -> NonNull seq
x <| y = ncons x (toNullable y)
