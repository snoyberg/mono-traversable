{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.LText
    ( LText
    ) where

import qualified Prelude
import Prelude ((.), Char)
import ClassyPrelude.Classes
import qualified Data.Text.Lazy as TL
import Data.Int (Int64)

type LText = TL.Text

instance (co ~ LText, i ~ Char, o ~ Char) => CanMap LText co i o where
    map = TL.map
instance (co ~ LText, i ~ Char, o ~ LText) => CanConcatMap LText co i o where
    concatMap = TL.concatMap
instance CanFilter LText Char where
    filter = TL.filter
instance CanSingleton LText Prelude.Char where
    singleton = TL.singleton
instance CanNull LText where
    null = TL.null
instance CanPack LText Prelude.Char where
    pack = TL.pack
    unpack = TL.unpack
instance CanEmpty LText where
    empty = TL.empty
instance CanStripPrefix LText where
    stripPrefix = TL.stripPrefix
instance CanBreak LText Prelude.Char where
    break = TL.break
    span = TL.span
    dropWhile = TL.dropWhile
    takeWhile = TL.takeWhile
instance CanAny LText Prelude.Char where
    any = TL.any
    all = TL.all
instance CanSplitAt LText Int64 where
    splitAt = TL.splitAt
