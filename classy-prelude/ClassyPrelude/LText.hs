{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.LText
    ( LText
    ) where

import qualified Prelude
import Prelude ((.), Char)
import ClassyPrelude.Classes
import qualified Data.Text.Lazy as TL
import Data.Int (Int64)
import qualified Data.Text

type LText = TL.Text

instance CanMapFunc LText LText Char Char where
    mapFunc = TL.map
instance CanConcatMapFunc LText LText Char LText where
    concatMapFunc = TL.concatMap
instance CanFilterFunc LText LText Char where
    filterFunc = TL.filter
instance CanSingleton LText Prelude.Char where
    singleton = TL.singleton
instance CanNull LText where
    null = TL.null
instance CanPack LText Prelude.Char where
    pack = TL.pack
    unpack = TL.unpack
instance CanStripPrefix LText where
    stripPrefix = TL.stripPrefix
    isPrefixOf = TL.isPrefixOf
instance CanBreak LText Prelude.Char where
    break = TL.break
    span = TL.span
    dropWhile = TL.dropWhile
    takeWhile = TL.takeWhile
instance CanAny LText Prelude.Char where
    any = TL.any
    all = TL.all
instance CanSplitAt LText Int64 where
    splitAt = TL.splitAt

instance CanWords LText where
    words = TL.words
    unwords = TL.unwords
    lines = TL.lines
    unlines = TL.unlines

instance CanSplit LText Char where
    split = TL.split

instance CanStripSuffix LText where
    stripSuffix = TL.stripSuffix
    isSuffixOf = TL.isSuffixOf

instance CanIsInfixOf LText where
    isInfixOf = TL.isInfixOf

instance CanReverse LText where
    reverse = TL.reverse

instance CanLength LText Int64 where
    length = TL.length

instance CanFoldFunc LText Char accum where
    foldFunc = TL.foldl'

instance CanReplicate LText LText Int64 where
    replicate = TL.replicate

instance CanToChunks LText Data.Text.Text where
    toChunks = TL.toChunks
    fromChunks = TL.fromChunks
