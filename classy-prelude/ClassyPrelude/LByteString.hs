{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.LByteString
    ( LByteString
    ) where

import qualified Prelude
import Prelude ((.))
import ClassyPrelude.Classes
import qualified Data.ByteString.Lazy as L
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Filesystem.Path.CurrentOS as F
import Data.Word (Word8)
import Data.Int (Int64)
import qualified Data.ByteString

type LByteString = L.ByteString

instance CanMapFunc LByteString LByteString Word8 Word8 where
    mapFunc = L.map
instance CanConcatMapFunc LByteString LByteString Word8 LByteString where
    concatMapFunc = L.concatMap
instance CanFilterFunc LByteString LByteString Word8 where
    filterFunc = L.filter
instance CanLength LByteString Int64 where
    length = L.length
instance CanSingleton LByteString Word8 where
    singleton = L.singleton
instance CanNull LByteString where
    null = L.null
instance CanPack LByteString Word8 where
    pack = L.pack
    unpack = L.unpack
instance MonadIO m => CanReadFile (m LByteString) where
    readFile = liftIO . L.readFile . F.encodeString
instance CanWriteFileFunc LByteString where
    writeFileFunc fp = liftIO . L.writeFile (F.encodeString fp)
instance CanBreak LByteString Word8 where
    break = L.break
    span = L.span
    dropWhile = L.dropWhile
    takeWhile = L.takeWhile
instance CanAny LByteString Word8 where
    any = L.any
    all = L.all
instance CanSplitAt LByteString Int64 where
    splitAt = L.splitAt

instance CanReverse LByteString where
    reverse = L.reverse

instance CanFoldFunc LByteString Word8 accum where
    foldFunc = L.foldl'

instance CanReplicate LByteString Word8 Int64 where
    replicate = L.replicate

instance CanToChunks LByteString Data.ByteString.ByteString where
    toChunks = L.toChunks
    fromChunks = L.fromChunks

instance CanStripSuffix LByteString where
    stripSuffix x y
        | x `L.isSuffixOf` y = Prelude.Just (L.take (L.length y Prelude.- L.length x) y)
        | Prelude.otherwise = Prelude.Nothing
    isSuffixOf = L.isSuffixOf

instance CanToStrict LByteString Data.ByteString.ByteString where
    toStrict = L.toStrict
    fromStrict = L.fromStrict