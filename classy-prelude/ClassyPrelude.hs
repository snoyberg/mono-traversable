{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude
    ( -- * CorePrelude
      module CorePrelude
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
      -- ** Map-like
    , lookup
    , insert
    , delete
      -- ** Set-like
    , member
      -- ** Text-like
    , show
      -- ** Files
    , readFile
    , writeFile
    ) where

import qualified Prelude
import qualified Data.Maybe

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
