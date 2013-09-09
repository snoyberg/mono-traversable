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
import           Prelude              (Bool, Char, flip, ($))

type family Element c
type instance Element (t a) = a
type instance Element S.ByteString = Word8
type instance Element L.ByteString = Word8
type instance Element T.Text = Char
type instance Element TL.Text = Char

class MonoFunctor c where
    cmap :: (Element c -> Element c) -> c -> c
instance Functor f => MonoFunctor (f a) where
    cmap = fmap
instance MonoFunctor S.ByteString where
    cmap = S.map
instance MonoFunctor L.ByteString where
    cmap = L.map
instance MonoFunctor T.Text where
    cmap = T.map
instance MonoFunctor TL.Text where
    cmap = TL.map

class MonoFoldable c where
    cfoldMap :: Monoid m => (Element c -> m) -> c -> m
    cfoldMap f = cfoldr (mappend . f) mempty

    cfoldr :: (Element c -> b -> b) -> b -> c -> b
    cfoldl' :: (a -> Element c -> a) -> a -> c -> a

    ctoList :: c -> [Element c]
    ctoList t = build (\ c n -> cfoldr c n t)
    
    call :: (Element c -> Bool) -> c -> Bool
    call f = getAll . cfoldMap (All . f)
    
    cany :: (Element c -> Bool) -> c -> Bool
    cany f = getAny . cfoldMap (Any . f)

instance F.Foldable t => MonoFoldable (t a) where
    cfoldMap = F.foldMap
    cfoldr = F.foldr
    cfoldl' = F.foldl'
instance MonoFoldable S.ByteString where
    cfoldr = S.foldr
    cfoldl' = S.foldl'
    ctoList = S.unpack
    call = S.all
    cany = S.any
instance MonoFoldable L.ByteString where
    cfoldr = L.foldr
    cfoldl' = L.foldl'
    ctoList = L.unpack
    call = L.all
    cany = L.any
instance MonoFoldable T.Text where
    cfoldr = T.foldr
    cfoldl' = T.foldl'
    ctoList = T.unpack
    call = T.all
    cany = T.any
instance MonoFoldable TL.Text where
    cfoldr = TL.foldr
    cfoldl' = TL.foldl'
    ctoList = TL.unpack
    call = TL.all
    cany = TL.any

ctraverse_ :: (MonoFoldable c, Applicative f) => (Element c -> f b) -> c -> f ()
ctraverse_ f = cfoldr ((*>) . f) (pure ())

cfor_ :: (MonoFoldable c, Applicative f) => c -> (Element c -> f b) -> f ()
cfor_ = flip ctraverse_

cmapM_ :: (MonoFoldable c, Monad m) => (Element c -> m b) -> c -> m ()
cmapM_ f = cfoldr ((>>) . f) (return ())

cforM_ :: (MonoFoldable c, Monad m) => c -> (Element c -> m b) -> m ()
cforM_ = flip cmapM_

class (MonoFoldable c, Monoid c) => MonoFoldableMonoid c where
    cconcatMap :: (Element c -> c) -> c -> c
    cconcatMap = cfoldMap
instance (F.Foldable t, Monoid (t a)) => MonoFoldableMonoid (t a)
instance MonoFoldableMonoid S.ByteString where
    cconcatMap = S.concatMap
instance MonoFoldableMonoid L.ByteString where
    cconcatMap = L.concatMap
instance MonoFoldableMonoid T.Text where
    cconcatMap = T.concatMap
instance MonoFoldableMonoid TL.Text where
    cconcatMap = TL.concatMap

class (MonoFunctor c, MonoFoldable c) => MonoTraversable c where
    ctraverse :: Applicative f => (Element c -> f (Element c)) -> c -> f c
    cmapM :: Monad m => (Element c -> m (Element c)) -> c -> m c
instance (Traversable t, Monoid (t a)) => MonoTraversable (t a) where
    ctraverse = traverse
    cmapM = mapM
instance MonoTraversable S.ByteString where
    ctraverse f = fmap S.pack . traverse f . S.unpack
    cmapM f = liftM S.pack . mapM f . S.unpack
instance MonoTraversable L.ByteString where
    ctraverse f = fmap L.pack . traverse f . L.unpack
    cmapM f = liftM L.pack . mapM f . L.unpack
instance MonoTraversable T.Text where
    ctraverse f = fmap T.pack . traverse f . T.unpack
    cmapM f = liftM T.pack . mapM f . T.unpack
instance MonoTraversable TL.Text where
    ctraverse f = fmap TL.pack . traverse f . TL.unpack
    cmapM f = liftM TL.pack . mapM f . TL.unpack

cfor :: (MonoTraversable c, Applicative f) => c -> (Element c -> f (Element c)) -> f c
cfor = flip ctraverse

cforM :: (MonoTraversable c, Monad f) => c -> (Element c -> f (Element c)) -> f c
cforM = flip cmapM

class MonoPointed c where
    cpoint :: Element c -> c
instance Pointed t => MonoPointed (t a) where
    cpoint = point
instance MonoPointed S.ByteString where
    cpoint = S.singleton
instance MonoPointed L.ByteString where
    cpoint = L.singleton
instance MonoPointed T.Text where
    cpoint = T.singleton
instance MonoPointed TL.Text where
    cpoint = TL.singleton

class (Monoid c, MonoPointed c) => FromList c where
    fromList :: [Element c] -> c
    fromList = mconcat . fmap cpoint
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
