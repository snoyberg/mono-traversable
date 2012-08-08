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

type LByteString = L.ByteString

instance (co ~ LByteString, i ~ Word8, o ~ Word8) => CanMapFunc LByteString co i o where
    mapFunc = L.map
instance (co ~ LByteString, i ~ Word8, o ~ LByteString) => CanConcatMapFunc LByteString co i o where
    concatMapFunc = L.concatMap
instance CanFilterFunc LByteString Word8 where
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
instance CanEmpty LByteString where
    empty = L.empty
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
