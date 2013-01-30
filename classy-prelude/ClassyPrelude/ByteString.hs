{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.ByteString
    ( ByteString
    ) where

import ClassyPrelude.Classes
import Prelude ((.), ($), otherwise, Maybe(..), Monad, Ord, Eq, Int, Bool)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.Word (Word8)
import qualified Prelude
import qualified Data.ByteString as ByteString
import qualified Filesystem.Path.CurrentOS as FilePath


instance CanMapFunc ByteString ByteString Word8 Word8 where
    mapFunc = ByteString.map

instance CanConcatMapFunc ByteString ByteString Word8 ByteString where
    concatMapFunc = ByteString.concatMap

instance CanFilterFunc ByteString ByteString Word8 where
    filterFunc = ByteString.filter

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

instance MonadIO m => CanReadFile (m ByteString) where
    readFile = liftIO . ByteString.readFile . FilePath.encodeString

instance CanWriteFileFunc ByteString where
    writeFileFunc fp = liftIO . ByteString.writeFile (FilePath.encodeString fp)

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

instance CanFoldFunc ByteString Word8 accum where
    foldFunc = ByteString.foldl'

instance CanReplicate ByteString Word8 Int where
    replicate = ByteString.replicate

instance CanStripSuffix ByteString where
    stripSuffix x y
        | x `ByteString.isSuffixOf` y = Just (ByteString.take (ByteString.length y Prelude.- ByteString.length x) y)
        | Prelude.otherwise = Nothing
    isSuffixOf = ByteString.isSuffixOf

instance MonadIO m => CanGetLine (m ByteString) where
    getLine = liftIO ByteString.getLine

instance CanPartition ByteString Word8 where
    partition = ByteString.partition
