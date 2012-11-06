{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
module ClassyPrelude.Conduit
    ( -- * Re-export
      module ClassyPrelude
    , module Data.Conduit
    , module Data.Conduit.List
      -- * XML
    , X.Document (..)
    , X.Name (..)
    , X.Prologue (..)
    , X.Node (..)
    , X.Element (..)
    ) where

import ClassyPrelude
import ClassyPrelude.Classes

import Data.Conduit
import Data.Conduit.List (consume, sinkNull)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Text as CT

import qualified Text.XML as X

instance MonadResource m => CanReadFile (Pipe l i ByteString u m ()) where
    readFile = CB.sourceFile . unpack
instance (u ~ r, MonadResource m) => CanWriteFile (Pipe l ByteString o u m r) where
    writeFile = CB.sinkFile . unpack

instance MonadIO m => CanReadFile (m X.Document) where
    readFile = liftIO . X.readFile X.def

instance CanWriteFileFunc X.Document where
    writeFileFunc fp = liftIO . X.writeFile X.def fp

instance (Monad m, i ~ i', o ~ o') => CanMap (Pipe l i o r m r) i' o' where
    map = CL.map
instance (Monad m, i ~ i', [o] ~ o') => CanConcatMap (Pipe l i o r m r) i' o' where
    concatMap = CL.concatMap
instance (Monad m, i ~ i', i ~ i'', r ~ r') => CanFilter (Pipe l i i' r m r') i'' where
    filter = CL.filter
instance (Monad m, i ~ i', o ~ o', m ~ m', r ~ r') => CanMapM (Pipe l i o r m r') m' i' o' where
    mapM = CL.mapM
instance (Monad m, i ~ i', m ~ m', r ~ r') => CanMapM_ (Pipe l i o r m r') m' i' where
    mapM_ f = awaitForever $ lift . f
instance (Monad m, i ~ i', r ~ r') => CanFold (Pipe l i o u m r) i' r' where
    fold = CL.fold

instance (MonadThrow m, i ~ Text, o ~ ByteString) => CanEncodeUtf8 (Pipe l i o r m r) where
    encodeUtf8 = CT.encode CT.utf8
instance (MonadThrow m, i ~ ByteString, o ~ Text) => CanDecodeUtf8 (Pipe l i o r m r) where
    decodeUtf8 = CT.decode CT.utf8
