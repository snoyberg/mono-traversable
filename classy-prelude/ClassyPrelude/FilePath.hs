{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.FilePath () where

import ClassyPrelude.Classes
import Prelude ((.), ($), otherwise, Maybe(..), Monad, Ord, Eq, Int, Bool, Char, Bool(..))
import Filesystem.Path.CurrentOS (FilePath)
import qualified Prelude
import qualified Data.List as List
import qualified Filesystem.Path.CurrentOS as FilePath


instance CanPack FilePath Char where
    pack = FilePath.decodeString
    unpack = FilePath.encodeString

instance CanIntersperse FilePath Char where
    intersperse c = pack . List.intersperse c . unpack

instance CanStripPrefix FilePath where
    stripPrefix = FilePath.stripPrefix
    isPrefixOf a b =
        case stripPrefix a b of
            Nothing -> False
            Just {} -> True
