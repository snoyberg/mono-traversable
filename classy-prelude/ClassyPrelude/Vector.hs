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

instance (i ~ a, co ~ Vector b) => CanMapFunc (Vector a) co i b where
    mapFunc = V.map
instance (i ~ a, co ~ Vector b) => CanConcatMapFunc (Vector a) co i (Vector b) where
    concatMapFunc = V.concatMap
instance CanFilterFunc (Vector a) a where
    filterFunc = V.filter
instance CanLength (Vector a) Prelude.Int where
    length = V.length
instance CanSingleton (Vector a) a where
    singleton = V.singleton
instance CanNull (Vector a) where
    null = V.null
instance CanPack (Vector a) a where
    pack = V.fromList
    unpack = V.toList
instance Prelude.Monad m => CanMapMFunc (Vector a) (Vector b) a b m where
    mapMFunc = V.mapM
instance Prelude.Monad m => CanMapM_Func (Vector a) a b m where
    mapM_Func = V.mapM_
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
instance CanFoldFunc (Vector a) accum a where
    foldFunc = V.foldl'
