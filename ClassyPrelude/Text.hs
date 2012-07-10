{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.Text
    ( Text
    ) where

import qualified Prelude
import Prelude ((.), Char)
import ClassyPrelude.Classes
import Data.Text (Text)
import qualified Data.Text as T

instance (co ~ Text, i ~ Char, o ~ Char) => CanMapFunc Text co i o where
    mapFunc = T.map
instance (co ~ Text, i ~ Char, o ~ Text) => CanConcatMapFunc Text co i o where
    concatMapFunc = T.concatMap
instance CanFilterFunc Text Char where
    filterFunc = T.filter
instance CanLength Text Prelude.Int where
    length = T.length
instance CanSingleton Text Prelude.Char where
    singleton = T.singleton
instance CanNull Text where
    null = T.null
instance CanPack Text Prelude.Char where
    pack = T.pack
    unpack = T.unpack
instance CanEmpty Text where
    empty = T.empty
instance CanStripPrefix Text where
    stripPrefix = T.stripPrefix
instance CanBreak Text Prelude.Char where
    break = T.break
    span = T.span
    dropWhile = T.dropWhile
    takeWhile = T.takeWhile
instance CanAny Text Prelude.Char where
    any = T.any
    all = T.all
instance CanSplitAt Text Prelude.Int where
    splitAt = T.splitAt
