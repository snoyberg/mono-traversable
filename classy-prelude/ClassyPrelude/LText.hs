{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.LText
    ( LText
    ) where

import CorePrelude
import ClassyPrelude.Classes
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.IO as LText
import qualified Data.Text.Lazy.Encoding as LText
import qualified Data.Text.Encoding.Error as Text


instance CanMap LText LText Char Char where
    map = LText.map

instance CanConcatMap LText LText Char LText where
    concatMap = LText.concatMap

instance CanFilter LText Char where
    filter = LText.filter

instance CanSingleton LText Char where
    singleton = LText.singleton

instance CanNull LText where
    null = LText.null

instance CanPack LText Char where
    pack = LText.pack
    unpack = LText.unpack

instance CanIntersperse LText Char where
    intersperse = LText.intersperse

instance CanStripPrefix LText where
    stripPrefix = LText.stripPrefix
    isPrefixOf = LText.isPrefixOf

instance CanBreak LText Char where
    break = LText.break
    span = LText.span
    dropWhile = LText.dropWhile
    takeWhile = LText.takeWhile

instance CanAny LText Char where
    any = LText.any
    all = LText.all

instance CanSplitAt LText Int64 where
    splitAt = LText.splitAt

instance CanWords LText where
    words = LText.words
    unwords = LText.unwords

instance CanLines LText where
    lines = LText.lines

instance CanUnlines LText where
    unlines = LText.unlines

instance CanSplit LText Char where
    split = LText.split

instance CanStripSuffix LText where
    stripSuffix = LText.stripSuffix
    isSuffixOf = LText.isSuffixOf

instance CanIsInfixOf LText where
    isInfixOf = LText.isInfixOf

instance CanReverse LText where
    reverse = LText.reverse

instance CanLength LText Int64 where
    length = LText.length

instance CanFold LText Char accum where
    fold = LText.foldl'

instance CanReplicate LText LText Int64 where
    replicate = LText.replicate

instance CanToChunks LText Text where
    toChunks = LText.toChunks
    fromChunks = LText.fromChunks

instance CanEncodeUtf8 LText LByteString where
    encodeUtf8 = LText.encodeUtf8

instance CanDecodeUtf8 LByteString LText where
    decodeUtf8 = LText.decodeUtf8With Text.lenientDecode

instance CanToStrict LText Text where
    toStrict = LText.toStrict
    fromStrict = LText.fromStrict

instance MonadIO m => CanGetLine (m LText) where
    getLine = liftIO LText.getLine

instance CanToLower LText where
    toLower = LText.toLower

instance CanToUpper LText where
    toUpper = LText.toUpper

instance CanToCaseFold LText where
    toCaseFold = LText.toCaseFold

instance CanFind LText Char where
    find = LText.find

instance CanPartition LText Char where
    partition = LText.partition

instance CanCons LText Char where
    cons = LText.cons

instance CanUncons LText Char where
    uncons = LText.uncons

instance CanCompareLength LText where
    compareLength c = LText.compareLength c . fromIntegral
