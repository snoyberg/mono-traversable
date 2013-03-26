{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.Maybe () where

import CorePrelude
import ClassyPrelude.Classes
import qualified Data.Maybe as Maybe
import qualified Control.Monad as Monad
import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable


instance CanMap (Maybe a) (Maybe b) a b where
  map = Monad.fmap

instance CanConcatMap (Maybe a) (Maybe b) a (Maybe b) where
  concatMap = (Monad.=<<)

instance CanFilter (Maybe a) a where
  filter = Monad.mfilter

instance CanLength (Maybe a) Int where
  length a = if null a then 0 else 1

instance CanSingleton (Maybe a) a where
  singleton = Monad.return

instance CanNull (Maybe a) where
  null = Maybe.isNothing

instance CanPack (Maybe a) a where
  pack = Maybe.listToMaybe
  unpack = Maybe.maybeToList

instance Monad m => CanMapM (Maybe i) (m (Maybe o)) m i o where
  mapM = Traversable.mapM

instance CanMapM_ (Maybe a) a where
  mapM_ = Foldable.mapM_

instance Eq x => CanMember (Maybe x) x where
  member = Foldable.elem

instance CanAny (Maybe a) a where
  any = Foldable.any
  all = Foldable.all

instance CanFold (Maybe a) a accum where
  fold = Foldable.foldl'

instance Eq a => CanIsInfixOf (Maybe a) where
  isInfixOf = (==)

instance CanReverse (Maybe a) where
  reverse = id

instance CanFind (Maybe a) a where
  find = Foldable.find

instance CanEmpty (Maybe a) where
  empty = Nothing
