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
import qualified Data.Conduit.Binary as CB

import qualified Text.XML as X

instance MonadResource m => CanReadFile (Pipe l i ByteString u m ()) where
    readFile = CB.sourceFile . unpack
instance (u ~ r, MonadResource m) => CanWriteFile (Pipe l ByteString o u m r) where
    writeFile = CB.sinkFile . unpack

instance MonadIO m => CanReadFile (m X.Document) where
    readFile = liftIO . X.readFile X.def

instance CanWriteFileFunc X.Document where
    writeFileFunc fp = liftIO . X.writeFile X.def fp
