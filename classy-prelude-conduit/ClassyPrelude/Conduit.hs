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
import qualified Data.Conduit.Binary as CB

import qualified Text.XML as X

instance CanReadFile X.Document where
    readFile = liftIO . X.readFile X.def

instance CanWriteFile X.Document where
    writeFile fp = liftIO . X.writeFile X.def fp
