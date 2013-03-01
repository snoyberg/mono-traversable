{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude
    ( -- * CorePrelude
      module CorePrelude
    , Seq
      -- * Standard
      -- ** Monoid
    , empty
    , append
    , (++)
      -- ** Monad
    , module Control.Monad
      -- ** Mutable references
    , module Control.Concurrent.MVar.Lifted
    , module Data.IORef.Lifted
      -- * Non-standard
      -- ** List-like classes
    , map
    , concat
    , concatMap
    , filter
    , find
    , length
    , singleton
    , null
    , pack
    , unpack
    , repack
    , fromList
    , toList
    , mapM
    , mapM_
    , forM
    , forM_
    , replicateM
    , stripPrefix
    , isPrefixOf
    , stripSuffix
    , isSuffixOf
    , isInfixOf
    , break
    , span
    , dropWhile
    , takeWhile
    , any
    , all
    , splitAt, take, drop
    , fold
    , words
    , unwords
    , lines
    , unlines
    , split
    , reverse
    , readMay
    , replicate
    , intercalate
    , intersperse
    , encodeUtf8
    , decodeUtf8
    , subsequences
    , permutations
    , partition
    , nub
    , nubBy
    , sort
    , sortBy
    , sortWith
    , group
    , groupBy
    , groupWith
    , cons
    , uncons
    , compareLength
    , Foldable.sum
    , Foldable.product
      -- ** Map-like
    , lookup
    , insert
    , delete
      -- ** Set-like
    , member
    , notMember
    , elem
    , notElem
    , union
    , difference
    , (\\)
    , intersection
    , intersect
    , unions
      -- ** Text-like
    , show
    , toLower
    , toUpper
    , toCaseFold
    , toStrict
    , fromStrict
      -- ** IO
    , readFile
    , writeFile
    , getLine
    , print
      -- ** Chunking
    , toChunks
    , fromChunks
      -- ** Force types
      -- | Helper functions for situations where type inferer gets confused.
    , asByteString
    , asLByteString
    , asHashMap
    , asHashSet
    , asText
    , asLText
    , asList
    , asMap
    , asMaybe
    , asSet
    , asVector
    ) where

import qualified Prelude
import Control.Monad (when, unless, void, liftM, ap, forever, join, sequence, sequence_)
import Control.Concurrent.MVar.Lifted
import Data.IORef.Lifted
import Data.Monoid (Monoid)
import qualified Data.Monoid as Monoid
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable

import CorePrelude hiding (print)
import ClassyPrelude.Classes

import ClassyPrelude.ByteString ()
import ClassyPrelude.Char ()
import ClassyPrelude.Classes ()
import ClassyPrelude.FilePath ()
import ClassyPrelude.HashMap ()
import ClassyPrelude.HashSet ()
import ClassyPrelude.LByteString ()
import ClassyPrelude.LText ()
import ClassyPrelude.List ()
import ClassyPrelude.Map ()
import ClassyPrelude.Maybe ()
import ClassyPrelude.Set ()
import ClassyPrelude.Text ()
import ClassyPrelude.Vector ()
import ClassyPrelude.Sequence (Seq)


show :: (Show a, CanPack c Char) => a -> c
show = pack . Prelude.show

fromList :: CanPack c i => [i] -> c
fromList = pack

toList :: CanPack c i => c -> [i]
toList = unpack

readMay :: (Read b, CanPack a Char) => a -> Maybe b
readMay a =
    case [x | (x, t) <- Prelude.reads (unpack a), null t] of
        [x] -> Just x
        _ -> Nothing

-- | Repack from one type to another, dropping to a list in the middle.
--
-- @repack = pack . unpack@.
repack :: (CanPack a i, CanPack b i) => a -> b
repack = pack . unpack

append :: Monoid m => m -> m -> m
append = mappend
{-# INLINE append #-}

empty :: Monoid m => m
empty = mempty
{-# INLINE empty #-}

infixr 5  ++
(++) :: Monoid m => m -> m -> m
(++) = mappend
{-# INLINE (++) #-}

infixl 9 \\{-This comment teaches CPP correct behaviour -}
-- | An alias for `difference`.
(\\) :: CanDifference c => c -> c -> c
(\\) = difference
{-# INLINE (\\) #-}

-- | An alias for `intersection`.
intersect :: CanIntersection c => c -> c -> c
intersect = intersection
{-# INLINE intersect #-}

unions :: (Foldable cc, Monoid c, CanUnion c) => cc c -> c
unions = Foldable.foldl' union Monoid.mempty

intercalate :: (CanConcat c i, CanIntersperse c i) => i -> c -> i
intercalate xs xss = concat (intersperse xs xss)

asByteString :: ByteString -> ByteString
asByteString = id

asLByteString :: LByteString -> LByteString
asLByteString = id

asHashMap :: HashMap k v -> HashMap k v
asHashMap = id

asHashSet :: HashSet a -> HashSet a
asHashSet = id

asText :: Text -> Text
asText = id

asLText :: LText -> LText
asLText = id

asList :: [a] -> [a]
asList = id

asMap :: Map k v -> Map k v
asMap = id

asMaybe :: Maybe a -> Maybe a
asMaybe = id

asSet :: Set a -> Set a
asSet = id

asVector :: Vector a -> Vector a
asVector = id

forM :: CanMapM ci mco m i o => ci -> (i -> m o) -> mco
forM = flip mapM

forM_ :: (Monad m, CanMapM_ ci i) => ci -> (i -> m o) -> m ()
forM_ = flip mapM_

-- | An alias for 'member'
elem :: CanMember c k => k -> c -> Bool
elem = member

-- | An alias for 'notMember'
notElem :: CanMember c k => k -> c -> Bool
notElem = notMember

print :: (Show a, MonadIO m) => a -> m ()
print = liftIO . Prelude.print

take :: CanSplitAt c i => i -> c -> c
take i c  = Prelude.fst (splitAt i c)

drop :: CanSplitAt c i => i -> c -> c
drop i c  = Prelude.snd (splitAt i c)

-- | Sort elements using the user supplied function to project something out of
-- each element.
-- Inspired by <http://hackage.haskell.org/packages/archive/base/latest/doc/html/GHC-Exts.html#v:sortWith>.
sortWith :: (CanSortBy c a, Ord b) => (a -> b) -> c -> c
sortWith f = sortBy $ comparing f

-- | The 'groupWith' function uses the user supplied function which
-- projects an element out of every list element in order to first sort the
-- input list and then to form groups by equality on these projected elements
--
-- Inspired by <http://hackage.haskell.org/packages/archive/base/latest/doc/html/GHC-Exts.html#v:groupWith>
groupWith :: (CanGroupBy c a, Eq b) => (a -> b) -> c -> [c]
groupWith f = groupBy (\a b -> f a == f b)
