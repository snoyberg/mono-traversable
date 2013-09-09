{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.MonoTraversable where

import           Control.Applicative
import           Control.Category
import           Control.Monad        (Monad (..), liftM, foldM)
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import           Data.Convertible
import           Data.Either
import qualified Data.Foldable        as F
import           Data.Functor
import           Data.Monoid
import           Data.Pointed
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL
import           Data.Traversable
import           Data.Word            (Word8)
import           GHC.Exts             (build)
import           Prelude              (Char, flip, ($))

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

class (MonoFunctor c, MonoFoldable c) => MonoTraversable c where
    mtraverse :: Applicative f => (Element c -> f (Element c)) -> c -> f c
    mmapM :: Monad m => (Element c -> m (Element c)) -> c -> m c
instance (Traversable t, Monoid (t a)) => MonoTraversable (t a) where
    mtraverse = traverse
    mmapM = mapM
instance MonoTraversable S.ByteString where
    mtraverse f = fmap S.pack . traverse f . S.unpack
    mmapM f = liftM S.pack . mapM f . S.unpack
instance MonoTraversable L.ByteString where
    mtraverse f = fmap L.pack . traverse f . L.unpack
    mmapM f = liftM L.pack . mapM f . L.unpack
instance MonoTraversable T.Text where
    mtraverse f = fmap T.pack . traverse f . T.unpack
    mmapM f = liftM T.pack . mapM f . T.unpack
instance MonoTraversable TL.Text where
    mtraverse f = fmap TL.pack . traverse f . TL.unpack
    mmapM f = liftM TL.pack . mapM f . TL.unpack

mfor :: (MonoTraversable c, Applicative f) => c -> (Element c -> f (Element c)) -> f c
mfor = flip mtraverse

mforM :: (MonoTraversable c, Monad f) => c -> (Element c -> f (Element c)) -> f c
mforM = flip mmapM

class MonoPointed c where
    mpoint :: Element c -> c
instance Pointed t => MonoPointed (t a) where
    mpoint = point
instance MonoPointed S.ByteString where
    mpoint = S.singleton
instance MonoPointed L.ByteString where
    mpoint = L.singleton
instance MonoPointed T.Text where
    mpoint = T.singleton
instance MonoPointed TL.Text where
    mpoint = TL.singleton

class (Monoid c, MonoPointed c) => FromList c where
    fromList :: [Element c] -> c
    fromList = mconcat . fmap mpoint
instance (Monoid (t a), Pointed t) => FromList (t a)
instance FromList S.ByteString where
    fromList = S.pack
instance FromList L.ByteString where
    fromList = L.pack
instance FromList T.Text where
    fromList = T.pack
instance FromList TL.Text where
    fromList = TL.pack

mconvert :: (MonoFunctor c, MonoFoldable c,
             MonoFunctor d, FromList d,
             Convertible (Element c) (Element d))
         => c -> d
mconvert = fromList . foldr ((:) . convert) []

msafeConvert :: (MonoFunctor c, MonoFoldable c,
             MonoFunctor d, FromList d,
             Convertible (Element c) (Element d))
         => c -> ConvertResult d
msafeConvert =
    fmap fromList . foldM (\acc x -> (:acc) <$> safeConvert x) [] . toList
