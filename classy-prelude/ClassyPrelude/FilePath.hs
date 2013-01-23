{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.FilePath () where

import qualified Prelude
import Prelude ((.), Char)
import ClassyPrelude.Classes
import qualified Data.List
import qualified Filesystem.Path.CurrentOS as F

instance CanPack F.FilePath Prelude.Char where
    pack = F.decodeString
    unpack = F.encodeString
instance CanIntersperse F.FilePath Prelude.Char where
    intersperse c = pack . Data.List.intersperse c . unpack
instance CanStripPrefix F.FilePath where
    stripPrefix = F.stripPrefix
    isPrefixOf a b =
        case stripPrefix a b of
            Prelude.Nothing -> Prelude.False
            Prelude.Just{} -> Prelude.True
