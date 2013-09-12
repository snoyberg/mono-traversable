{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
module ClassyPrelude.Classes where

import CorePrelude
import qualified Data.List as List
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.IO as LText
import qualified Filesystem.Path.CurrentOS as FilePath
import qualified Data.Vector as Vector
import qualified Data.Vector.Unboxed as UVector
import qualified Data.Sequence as Seq
import Data.MonoTraversable
import Data.Sequences (fromStrict)
import Control.Monad (liftM)
import System.IO (Handle)

class IOData a where
    readFile :: MonadIO m => FilePath -> m a
    writeFile :: MonadIO m => FilePath -> a -> m ()
    getLine :: MonadIO m => m a
    hGetContents :: MonadIO m => Handle -> m a
    hGetLine :: MonadIO m => Handle -> m a
    hPut :: MonadIO m => Handle -> a -> m ()
instance IOData ByteString where
    readFile = liftIO . ByteString.readFile . FilePath.encodeString
    writeFile fp = liftIO . ByteString.writeFile (FilePath.encodeString fp)
    getLine = liftIO ByteString.getLine
    hGetContents = liftIO . ByteString.hGetContents
    hGetLine = liftIO . ByteString.hGetLine
    hPut h = liftIO . ByteString.hPut h
instance IOData LByteString where
    readFile = liftIO . LByteString.readFile . FilePath.encodeString
    writeFile fp = liftIO . LByteString.writeFile (FilePath.encodeString fp)
    getLine = liftM fromStrict (liftIO ByteString.getLine)
    hGetContents = liftIO . LByteString.hGetContents
    hGetLine = liftM fromStrict . liftIO . ByteString.hGetLine
    hPut h = liftIO . LByteString.hPut h
instance IOData Text where
    readFile = liftIO . Text.readFile . FilePath.encodeString
    writeFile fp = liftIO . Text.writeFile (FilePath.encodeString fp)
    getLine = liftIO Text.getLine
    hGetContents = liftIO . Text.hGetContents
    hGetLine = liftIO . Text.hGetLine
    hPut h = liftIO . Text.hPutStr h
instance IOData LText where
    readFile = liftIO . LText.readFile . FilePath.encodeString
    writeFile fp = liftIO . LText.writeFile (FilePath.encodeString fp)
    getLine = liftIO LText.getLine
    hGetContents = liftIO . LText.hGetContents
    hGetLine = liftIO . LText.hGetLine
    hPut h = liftIO . LText.hPutStr h

class CanZip c1 c2 withRes t | c1 -> c2 withRes t , c2 -> c1 where
    zip :: c1 -> c2 -> t (Element c1, Element c2)
    unzip :: t (Element c1, Element c2) -> (c1, c2)
    zipWith :: (Element c1 -> Element c2 -> Element withRes) -> c1 -> c2 -> withRes
instance CanZip [a] [b] [c] [] where
    zip = List.zip
    unzip = List.unzip
    zipWith = List.zipWith
instance CanZip (Vector a) (Vector b) (Vector c) Vector where
    zip = Vector.zip
    unzip = Vector.unzip
    zipWith = Vector.zipWith
instance (Unbox a, Unbox b, Unbox c) => CanZip (UVector a) (UVector b) (UVector c) UVector where
    zip = UVector.zip
    unzip = UVector.unzip
    zipWith = UVector.zipWith
instance CanZip (Seq a) (Seq b) (Seq c) Seq where
    zip = Seq.zip
    unzip = (\(a, b) -> (Seq.fromList a, Seq.fromList b)) . List.unzip . otoList
    zipWith = Seq.zipWith
instance CanZip ByteString ByteString [a] [] where
    zip = ByteString.zip
    unzip = ByteString.unzip
    zipWith = ByteString.zipWith
instance CanZip LByteString LByteString [a] [] where
    zip = LByteString.zip
    unzip = LByteString.unzip
    zipWith = LByteString.zipWith
instance CanZip Text Text Text [] where
    zip = Text.zip
    unzip = (Text.pack *** Text.pack) . List.unzip
    zipWith = Text.zipWith
instance CanZip LText LText LText [] where
    zip = LText.zip
    unzip = (LText.pack *** LText.pack) . List.unzip
    zipWith = LText.zipWith

class CanZip3 t a b c d | t -> a b c d where
    zip3 :: t a -> t b -> t c -> t (a, b, c)
    unzip3 :: t (a, b, c) -> (t a, t b, t c)
    zipWith3 :: (a -> b -> c -> d) -> t a -> t b -> t c -> t d
instance CanZip3 [] a b c d where
    zip3 = List.zip3
    unzip3 = List.unzip3
    zipWith3 = List.zipWith3
instance CanZip3 Vector a b c d where
    zip3 = Vector.zip3
    unzip3 = Vector.unzip3
    zipWith3 = Vector.zipWith3
instance (Unbox a, Unbox b, Unbox c, Unbox d) => CanZip3 UVector a b c d where
    zip3 = UVector.zip3
    unzip3 = UVector.unzip3
    zipWith3 = UVector.zipWith3
instance CanZip3 Seq a b c d where
    zip3 = Seq.zip3
    unzip3 = (\(a, b, c) -> (Seq.fromList a, Seq.fromList b, Seq.fromList c)) . List.unzip3 . otoList
    zipWith3 = Seq.zipWith3

class CanZip4 t a b c d e | t -> a b c d e where
    zip4 :: t a -> t b -> t c -> t d -> t (a, b, c, d)
    unzip4 :: t (a, b, c, d) -> (t a, t b, t c, t d)
    zipWith4 :: (a -> b -> c -> d -> e) -> t a -> t b -> t c -> t d -> t e
instance CanZip4 [] a b c d e where
    zip4 = List.zip4
    unzip4 = List.unzip4
    zipWith4 = List.zipWith4
instance CanZip4 Vector a b c d e where
    zip4 = Vector.zip4
    unzip4 = Vector.unzip4
    zipWith4 = Vector.zipWith4
instance (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e) => CanZip4 UVector a b c d e where
    zip4 = UVector.zip4
    unzip4 = UVector.unzip4
    zipWith4 = UVector.zipWith4
instance CanZip4 Seq a b c d e where
    zip4 = Seq.zip4
    unzip4 = (\(a, b, c, d) -> (Seq.fromList a, Seq.fromList b, Seq.fromList c, Seq.fromList d)) . List.unzip4 . otoList
    zipWith4 = Seq.zipWith4

class CanZip5 t a b c d e f | t -> a b c d e f where
    zip5 :: t a -> t b -> t c -> t d -> t e -> t (a, b, c, d, e)
    unzip5 :: t (a, b, c, d, e) -> (t a, t b, t c, t d, t e)
    zipWith5 :: (a -> b -> c -> d -> e -> f) -> t a -> t b -> t c -> t d -> t e -> t f
instance CanZip5 [] a b c d e f where
    zip5 = List.zip5
    unzip5 = List.unzip5
    zipWith5 = List.zipWith5
instance CanZip5 Vector a b c d e f where
    zip5 = Vector.zip5
    unzip5 = Vector.unzip5
    zipWith5 = Vector.zipWith5
instance (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f) => CanZip5 UVector a b c d e f where
    zip5 = UVector.zip5
    unzip5 = UVector.unzip5
    zipWith5 = UVector.zipWith5

class CanZip6 t a b c d e f g | t -> a b c d e f g where
    zip6 :: t a -> t b -> t c -> t d -> t e -> t f -> t (a, b, c, d, e, f)
    unzip6 :: t (a, b, c, d, e, f) -> (t a, t b, t c, t d, t e, t f)
    zipWith6 :: (a -> b -> c -> d -> e -> f -> g) -> t a -> t b -> t c -> t d -> t e -> t f -> t g
instance CanZip6 [] a b c d e f g where
    zip6 = List.zip6
    unzip6 = List.unzip6
    zipWith6 = List.zipWith6
instance CanZip6 Vector a b c d e f g where
    zip6 = Vector.zip6
    unzip6 = Vector.unzip6
    zipWith6 = Vector.zipWith6
instance (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f, Unbox g) => CanZip6 UVector a b c d e f g where
    zip6 = UVector.zip6
    unzip6 = UVector.unzip6
    zipWith6 = UVector.zipWith6

class CanZip7 t a b c d e f g h | t -> a b c d e f g h where
    zip7 :: t a -> t b -> t c -> t d -> t e -> t f -> t g -> t (a, b, c, d, e, f, g)
    unzip7 :: t (a, b, c, d, e, f, g) -> (t a, t b, t c, t d, t e, t f, t g)
    zipWith7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> t a -> t b -> t c -> t d -> t e -> t f -> t g -> t h
instance CanZip7 [] a b c d e f g h where
    zip7 = List.zip7
    unzip7 = List.unzip7
    zipWith7 = List.zipWith7
