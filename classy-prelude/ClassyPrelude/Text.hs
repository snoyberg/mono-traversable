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
import qualified Data.Text.Encoding
import qualified Data.Text.Encoding.Error
import Data.ByteString (ByteString)
import qualified Data.Text.IO as T
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Filesystem.Path.CurrentOS as F

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
instance CanIntersperse Text Prelude.Char where
    intersperse = T.intersperse
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

instance CanLinesFunc Text where
    linesFunc = T.lines

instance CanUnlines Text where
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

instance CanFoldFunc Text Char accum where
    foldFunc = T.foldl'

instance CanReplicate Text Text Prelude.Int where
    replicate = T.replicate

instance CanEncodeUtf8Func Text ByteString where
    encodeUtf8Func = Data.Text.Encoding.encodeUtf8
instance CanDecodeUtf8Func ByteString Text where
    decodeUtf8Func = Data.Text.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode

instance MonadIO m => CanGetLine (m Text) where
    getLine = liftIO T.getLine

instance CanToLower Text where
    toLower = T.toLower

instance CanToUpper Text where
    toUpper = T.toUpper

instance CanToCaseFold Text where
    toCaseFold = T.toCaseFold

instance CanFind Text Prelude.Char where
    find = T.find

instance CanPartition Text Prelude.Char where
    partition = T.partition
