{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.Text
    ( Text
    ) where

import qualified Prelude
import Prelude ((.), Char)
import ClassyPrelude.Classes
import Data.Text (Text)
import qualified Data.Text as T

instance CanMapFunc Text Text Char Char where
    mapFunc = T.map
instance CanConcatMapFunc Text Text Char Text where
    concatMapFunc = T.concatMap
instance CanFilterFunc Text Text Char where
    filterFunc = T.filter
instance CanLength Text Prelude.Int where
    length = T.length
instance CanSingleton Text Prelude.Char where
    singleton = T.singleton
instance CanNull Text where
    null = T.null
instance CanPack Text Prelude.Char where
    pack = T.pack
    unpack = T.unpack
instance CanEmpty Text where
    empty = T.empty
instance CanStripPrefix Text where
    stripPrefix = T.stripPrefix
    isPrefixOf = T.isPrefixOf
instance CanBreak Text Prelude.Char where
    break = T.break
    span = T.span
    dropWhile = T.dropWhile
    takeWhile = T.takeWhile
instance CanAny Text Prelude.Char where
    any = T.any
    all = T.all
instance CanSplitAt Text Prelude.Int where
    splitAt = T.splitAt

instance CanWords Text where
    words = T.words
    unwords = T.unwords
    lines = T.lines
    unlines = T.unlines

instance CanSplit Text Char where
    split = T.split

instance CanStripSuffix Text where
    stripSuffix = T.stripSuffix
    isSuffixOf = T.isSuffixOf

instance CanIsInfixOf Text where
    isInfixOf = T.isInfixOf

instance CanReverse Text where
    reverse = T.reverse
