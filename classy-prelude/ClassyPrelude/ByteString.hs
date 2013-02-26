{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.ByteString
    ( ByteString
    ) where

import CorePrelude
import ClassyPrelude.Classes
import qualified Data.ByteString as ByteString
import qualified Filesystem.Path.CurrentOS as FilePath


instance CanMap ByteString ByteString Word8 Word8 where
    map = ByteString.map

instance CanConcatMap ByteString ByteString Word8 ByteString where
    concatMap = ByteString.concatMap

instance CanFilter ByteString Word8 where
    filter = ByteString.filter

instance CanLength ByteString Int where
    length = ByteString.length

instance CanSingleton ByteString Word8 where
    singleton = ByteString.singleton

instance CanNull ByteString where
    null = ByteString.null

instance CanPack ByteString Word8 where
    pack = ByteString.pack
    unpack = ByteString.unpack

instance CanIntersperse ByteString Word8 where
    intersperse = ByteString.intersperse

instance CanStripPrefix ByteString where
    stripPrefix x y
        | x `ByteString.isPrefixOf` y =
            Just $ ByteString.drop (ByteString.length x) y
        | otherwise = Nothing
    isPrefixOf = ByteString.isPrefixOf

instance CanReadFile ByteString where
    readFile = liftIO . ByteString.readFile . FilePath.encodeString

instance CanWriteFile ByteString where
    writeFile fp = liftIO . ByteString.writeFile (FilePath.encodeString fp)

instance CanBreak ByteString Word8 where
    break = ByteString.break
    span = ByteString.span
    dropWhile = ByteString.dropWhile
    takeWhile = ByteString.takeWhile

instance CanAny ByteString Word8 where
    any = ByteString.any
    all = ByteString.all

instance CanSplitAt ByteString Int where
    splitAt = ByteString.splitAt

instance CanReverse ByteString where
    reverse = ByteString.reverse

instance CanFold ByteString Word8 accum where
    fold = ByteString.foldl'

instance CanReplicate ByteString Word8 Int where
    replicate = ByteString.replicate

instance CanStripSuffix ByteString where
    stripSuffix x y
        | x `ByteString.isSuffixOf` y = Just (ByteString.take (ByteString.length y - ByteString.length x) y)
        | otherwise = Nothing
    isSuffixOf = ByteString.isSuffixOf

instance MonadIO m => CanGetLine (m ByteString) where
    getLine = liftIO ByteString.getLine

instance CanPartition ByteString Word8 where
    partition = ByteString.partition

instance CanCons ByteString Word8 where
    cons = ByteString.cons

instance CanUncons ByteString Word8 where
    uncons = ByteString.uncons

instance CanGroupBy ByteString Word8 where
    groupBy = ByteString.groupBy
