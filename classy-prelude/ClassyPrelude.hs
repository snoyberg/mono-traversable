{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude
    ( -- * CorePrelude
      module CorePrelude
      -- * Standard
      -- ** Monoid
    , concat
    , empty
    , append
    , (++)
      -- ** Monad
    , module Control.Monad
      -- * Non-standard
      -- ** List-like classes
    , map
    , concatMap
    , filter
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
    , encodeUtf8
    , decodeUtf8
      -- ** Map-like
    , lookup
    , insert
    , delete
      -- ** Set-like
    , member
    , elem
      -- ** Text-like
    , show
    , toLower
    , toUpper
    , toCaseFold
      -- ** IO
    , readFile
    , writeFile
    , getLine
      -- ** Chunking
    , toChunks
    , fromChunks
      -- ** Force types
    , asByteString
    , asLByteString
    , asHashMap
    , asHashSet
    , asText
    , asLText
    , asList
    , asMap
    , asSet
    , asVector
    ) where

import qualified Prelude
import qualified Data.Maybe
import Control.Monad (when, unless, void, liftM, ap, forever)

import CorePrelude
import ClassyPrelude.Classes

import ClassyPrelude.ByteString ()
import ClassyPrelude.Classes ()
import ClassyPrelude.FilePath ()
import ClassyPrelude.HashMap ()
import ClassyPrelude.HashSet ()
import ClassyPrelude.LByteString ()
import ClassyPrelude.LText ()
import ClassyPrelude.List ()
import ClassyPrelude.Map ()
import ClassyPrelude.Set ()
import ClassyPrelude.Text ()
import ClassyPrelude.Vector ()

show :: (Prelude.Show a, CanPack c Char) => a -> c
show = pack . Prelude.show

fromList :: CanPack c i => [i] -> c
fromList = pack

toList :: CanPack c i => c -> [i]
toList = unpack

-- Misc instances
instance CanPack (Prelude.Maybe a) a where
    pack = Data.Maybe.listToMaybe
    unpack = Data.Maybe.maybeToList

readMay :: (Prelude.Read b, CanPack a Char) => a -> Maybe b
readMay a =
    case [x | (x, t) <- Prelude.reads (unpack a), null t] of
        [x] -> Just x
        _ -> Nothing

-- | Repack from one type to another, dropping to a list in the middle.
--
-- @repack = pack . unpack@.
repack :: (CanPack a i, CanPack b i) => a -> b
repack = pack . unpack

concat :: Monoid m => [m] -> m
concat = mconcat
{-# INLINE concat #-}

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

asSet :: Set a -> Set a
asSet = id

asVector :: Vector a -> Vector a
asVector = id

forM :: CanMapMFunc ci mco m i o => ci -> (i -> m o) -> mco
forM = flip mapM

forM_ :: (Monad m, CanMapM_Func ci i) => ci -> (i -> m o) -> m ()
forM_ = flip mapM_

elem :: CanMember c k => k -> c -> Bool
elem = member
