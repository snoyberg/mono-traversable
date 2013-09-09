{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.MonoTraversable where

import Data.Monoid
import qualified Data.Foldable as F
import Data.Traversable
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Word (Word8)
import Control.Applicative
import Data.Functor
import Prelude (Char, flip, Monad (..))
import Control.Category
import GHC.Exts (build)

type family Element c
type instance Element (t a) = a
type instance Element S.ByteString = Word8
type instance Element L.ByteString = Word8
type instance Element T.Text = Char
type instance Element TL.Text = Char

class MonoFunctor c where
    mmap :: (Element c -> Element c) -> c -> c
instance Functor f => MonoFunctor (f a) where
    mmap = fmap
instance MonoFunctor S.ByteString where
    mmap = S.map
instance MonoFunctor L.ByteString where
    mmap = L.map
instance MonoFunctor T.Text where
    mmap = T.map
instance MonoFunctor TL.Text where
    mmap = TL.map

class Monoid c => MonoFoldable c where
    foldMap :: Monoid m => (Element c -> m) -> c -> m
    foldMap f = foldl' (\a b -> a `mappend` f b) mempty

    foldr :: (Element c -> b -> b) -> b -> c -> b
    foldl' :: (a -> Element c -> a) -> a -> c -> a
    mconcatMap :: (Element c -> c) -> c -> c

    toList :: c -> [Element c]
    toList t = build (\ c n -> foldr c n t)
    {-# INLINE toList #-}

instance (F.Foldable t, Monoid (t a)) => MonoFoldable (t a) where
    foldMap = F.foldMap
    foldr = F.foldr
    foldl' = F.foldl'
    mconcatMap = F.foldMap
instance MonoFoldable S.ByteString where
    foldr = S.foldr
    foldl' = S.foldl'
    mconcatMap = S.concatMap
    toList = S.unpack
instance MonoFoldable L.ByteString where
    foldr = L.foldr
    foldl' = L.foldl'
    mconcatMap = L.concatMap
    toList = L.unpack
instance MonoFoldable T.Text where
    foldr = T.foldr
    foldl' = T.foldl'
    mconcatMap = T.concatMap
    toList = T.unpack
instance MonoFoldable TL.Text where
    foldr = TL.foldr
    foldl' = TL.foldl'
    mconcatMap = TL.concatMap
    toList = TL.unpack

traverse_ :: (MonoFoldable c, Applicative f) => (Element c -> f b) -> c -> f ()
traverse_ f = foldr ((*>) . f) (pure ())

for_ :: (MonoFoldable c, Applicative f) => c -> (Element c -> f b) -> f ()
for_ = flip traverse_

mapM_ :: (MonoFoldable c, Monad m) => (Element c -> m b) -> c -> m ()
mapM_ f = foldr ((>>) . f) (return ())

forM_ :: (MonoFoldable c, Monad m) => c -> (Element c -> m b) -> m ()
forM_ = flip mapM_