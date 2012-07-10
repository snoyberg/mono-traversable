{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
module ClassyPrelude.Conduit
    ( -- * Re-export
      module ClassyPrelude
    , module Data.Conduit
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
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB

import qualified Text.XML as X

instance Monad m => CanMap (Pipe l i o r m r) i o where
    map = CL.map
instance (Monad m, r ~ r') => CanFilter (Pipe l i i r m r') i where
    filter = CL.filter
instance (i ~ i', Monad m, m ~ m', u ~ r, o' ~ ()) => CanMapM_ (Pipe l i o u m r) i' o' m' where
    mapM_ = CL.mapM_
instance MonadResource m => CanReadFile (Pipe l i ByteString u m ()) where
    readFile = CB.sourceFile . unpack
instance (u ~ r, MonadResource m) => CanWriteFile (Pipe l ByteString o u m r) where
    writeFile = CB.sinkFile . unpack
instance (Monad m, accum ~ accum') => CanFold accum a (Pipe l a o u m accum') where
    fold = CL.fold

instance MonadIO m => CanReadFile (m X.Document) where
    readFile = liftIO . X.readFile X.def

instance CanWriteFileFunc X.Document where
    writeFileFunc fp = liftIO . X.writeFile X.def fp
