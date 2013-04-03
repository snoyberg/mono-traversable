{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.LByteString
    ( LByteString
    ) where

import CorePrelude
import ClassyPrelude.Classes
import qualified Data.ByteString as ByteString
import qualified Filesystem.Path.CurrentOS as FilePath
import qualified Data.ByteString.Lazy as LByteString


instance CanMap LByteString LByteString Word8 Word8 where
    map = LByteString.map

instance CanConcatMap LByteString LByteString Word8 LByteString where
    concatMap = LByteString.concatMap

instance CanFilter LByteString Word8 where
    filter = LByteString.filter

instance CanLength LByteString Int64 where
    length = LByteString.length

instance CanSingleton LByteString Word8 where
    singleton = LByteString.singleton

instance CanNull LByteString where
    null = LByteString.null

instance CanPack LByteString Word8 where
    pack = LByteString.pack
    unpack = LByteString.unpack

instance CanIntersperse LByteString Word8 where
    intersperse = LByteString.intersperse

instance CanStripPrefix LByteString where
    stripPrefix x y
        | x `LByteString.isPrefixOf` y =
            Just $ LByteString.drop (LByteString.length x) y
        | otherwise = Nothing
    isPrefixOf = LByteString.isPrefixOf

instance CanReadFile LByteString where
    readFile = liftIO . LByteString.readFile . FilePath.encodeString

instance CanWriteFile LByteString where
    writeFile fp = liftIO . LByteString.writeFile (FilePath.encodeString fp)

instance CanBreak LByteString Word8 where
    break = LByteString.break
    span = LByteString.span
    dropWhile = LByteString.dropWhile
    takeWhile = LByteString.takeWhile

instance CanAny LByteString Word8 where
    any = LByteString.any
    all = LByteString.all

instance CanSplitAt LByteString Int64 where
    splitAt = LByteString.splitAt

instance CanReverse LByteString where
    reverse = LByteString.reverse

instance CanFold LByteString Word8 accum where
    fold = LByteString.foldl'

instance CanReplicate LByteString Word8 Int64 where
    replicate = LByteString.replicate

instance CanToChunks LByteString ByteString where
    toChunks = LByteString.toChunks
    fromChunks = LByteString.fromChunks

instance CanStripSuffix LByteString where
    stripSuffix x y
        | x `LByteString.isSuffixOf` y = Just (LByteString.take (LByteString.length y - LByteString.length x) y)
        | otherwise = Nothing
    isSuffixOf = LByteString.isSuffixOf

instance CanToStrict LByteString ByteString where
    toStrict = ByteString.concat . toChunks
    fromStrict = fromChunks . return

instance CanPartition LByteString Word8 where
    partition = LByteString.partition

instance CanCons LByteString Word8 where
    cons = LByteString.cons

instance CanUncons LByteString Word8 where
    uncons = LByteString.uncons

instance CanGroupBy LByteString Word8 where
    groupBy = LByteString.groupBy

instance CanGroup LByteString Word8 where
    group = LByteString.group

instance CanRepeat LByteString Word8 where
    repeat = LByteString.repeat

instance CanZipWith LByteString Word8 LByteString Word8 [a] a where
    zipWith = LByteString.zipWith

instance CanZip LByteString Word8 LByteString Word8 [] where
    zip = LByteString.zip

instance CanUnzip LByteString Word8 LByteString Word8 [] where
    unzip = LByteString.unzip

instance CanEmpty LByteString
