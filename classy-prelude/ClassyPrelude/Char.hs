{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.Char where

import CorePrelude
import ClassyPrelude.Classes
import qualified Data.Char as Char

instance CanToLower Char where
    toLower = Char.toLower

instance CanToUpper Char where
    toUpper = Char.toUpper
