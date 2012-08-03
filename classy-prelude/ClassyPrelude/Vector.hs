{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.Vector
    ( Vector
    ) where

import qualified Prelude
import Prelude ((.))
import ClassyPrelude.Classes
import Data.Vector (Vector)
import qualified Data.Vector as V

instance (i ~ a, co ~ Vector b) => CanMap (Vector a) co i b where
    map = V.map
instance (i ~ a, co ~ Vector b) => CanConcatMap (Vector a) co i (Vector b) where
    concatMap = V.concatMap
instance CanFilter (Vector a) a where
    filter = V.filter
instance CanLength (Vector a) Prelude.Int where
    length = V.length
instance CanSingleton (Vector a) a where
    singleton = V.singleton
instance CanNull (Vector a) where
    null = V.null
instance CanPack (Vector a) a where
    pack = V.fromList
    unpack = V.toList
instance (a ~ a', b' ~ Vector b) => CanMapM (Vector a) b' a' b where
    mapM = V.mapM
instance a ~ a' => CanMapM_ (Vector a) a' where
    mapM_ = V.mapM_
instance CanEmpty (Vector a) where
    empty = V.empty
instance Prelude.Eq x => CanMember (Vector x) x where
    member x = V.any (Prelude.== x)
instance CanBreak (Vector a) a where
    break = V.break
    span = V.span
    dropWhile = V.dropWhile
    takeWhile = V.takeWhile
instance CanAny (Vector a) a where
    any = V.any
    all = V.all
instance CanSplitAt (Vector a) Prelude.Int where
    splitAt = V.splitAt
instance CanFold (Vector a) a accum where
    fold = V.foldl'
