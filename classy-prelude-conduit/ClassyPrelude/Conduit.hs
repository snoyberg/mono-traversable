{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
module ClassyPrelude.Conduit
    ( -- * Re-export
      module ClassyPrelude
    , module Data.Conduit
    , module Data.Conduit.List
    ) where

import ClassyPrelude

import Data.Conduit
import Data.Conduit.List (consume, sinkNull)
