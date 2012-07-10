{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.ByteString
    ( ByteString
    ) where

import qualified Prelude
import Prelude ((.))
import ClassyPrelude.Classes
import qualified Data.ByteString as S
import Data.ByteString (ByteString)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Filesystem.Path.CurrentOS as F
import Data.Word (Word8)

instance (co ~ ByteString, i ~ Word8, o ~ Word8) => CanMapFunc ByteString co i o where
    mapFunc = S.map
instance (co ~ ByteString, i ~ Word8, o ~ ByteString) => CanConcatMapFunc ByteString co i o where
    concatMapFunc = S.concatMap
instance CanFilterFunc ByteString Word8 where
    filterFunc = S.filter
instance CanLength ByteString Prelude.Int where
    length = S.length
instance CanSingleton ByteString Word8 where
    singleton = S.singleton
instance CanNull ByteString where
    null = S.null
instance CanPack ByteString Word8 where
    pack = S.pack
    unpack = S.unpack
instance CanEmpty ByteString where
    empty = S.empty
instance MonadIO m => CanReadFile (m ByteString) where
    readFile = liftIO . S.readFile . F.encodeString
instance CanWriteFileFunc ByteString where
    writeFileFunc fp = liftIO . S.writeFile (F.encodeString fp)
instance CanBreak ByteString Word8 where
    break = S.break
    span = S.span
    dropWhile = S.dropWhile
    takeWhile = S.takeWhile
instance CanAny ByteString Word8 where
    any = S.any
    all = S.all
instance CanSplitAt ByteString Prelude.Int where
    splitAt = S.splitAt
