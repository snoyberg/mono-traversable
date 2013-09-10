{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.MonoTraversable where

import           Control.Applicative
import           Control.Category
import           Control.Monad        (Monad (..), liftM)
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Foldable        as F
import           Data.Functor
import           Data.Monoid (Monoid (..), Any (..), All (..))
import           Data.Pointed
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL
import           Data.Traversable
import           Data.Word            (Word8)
import           GHC.Exts             (build)
import           Prelude              (Bool (..), const, Char, flip, ($), IO, Maybe, Either)
import Control.Arrow (Arrow)
import Data.Tree (Tree)
import Data.Sequence (Seq, ViewL, ViewR)
import Data.IntMap (IntMap)
import Data.Semigroup (Option)
import Data.List.NonEmpty (NonEmpty)
import Data.Functor.Identity (Identity)
import Data.Map (Map)
import Data.HashMap.Strict (HashMap)
import Data.Vector (Vector)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.List (ListT)
import Control.Monad.Trans.Identity (IdentityT)
import Data.Functor.Apply (MaybeApply, WrappedApplicative)
import Control.Comonad (Cokleisli)
import Control.Monad.Trans.Writer (WriterT)
import qualified Control.Monad.Trans.Writer.Strict as Strict (WriterT)
import Control.Monad.Trans.State (StateT)
import qualified Control.Monad.Trans.State.Strict as Strict (StateT)
import Control.Monad.Trans.RWS (RWST)
import qualified Control.Monad.Trans.RWS.Strict as Strict (RWST)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Error (ErrorT)
import Control.Monad.Trans.Cont (ContT)
import Data.Functor.Compose (Compose)
import Data.Functor.Product (Product)
import Data.Semigroupoid.Static (Static)

type family Element c
type instance Element S.ByteString = Word8
type instance Element L.ByteString = Word8
type instance Element T.Text = Char
type instance Element TL.Text = Char
type instance Element [a] = a
type instance Element (IO a) = a
type instance Element (ZipList a) = a
type instance Element (Maybe a) = a
type instance Element (Tree a) = a
type instance Element (Seq a) = a
type instance Element (ViewL a) = a
type instance Element (ViewR a) = a
type instance Element (IntMap a) = a
type instance Element (Option a) = a
type instance Element (NonEmpty a) = a
type instance Element (Identity a) = a
type instance Element (r -> a) = a
type instance Element (Either a b) = b
type instance Element (a, b) = b
type instance Element (Const m a) = a
type instance Element (WrappedMonad m a) = a
type instance Element (Map k v) = v
type instance Element (HashMap k v) = v
type instance Element (Vector a) = a
type instance Element (WrappedArrow a b c) = c
type instance Element (MaybeApply f a) = a
type instance Element (WrappedApplicative f a) = a
type instance Element (Cokleisli w a b) = b
type instance Element (MaybeT m a) = a
type instance Element (ListT m a) = a
type instance Element (IdentityT m a) = a
type instance Element (WriterT w m a) = a
type instance Element (Strict.WriterT w m a) = a
type instance Element (StateT s m a) = a
type instance Element (Strict.StateT s m a) = a
type instance Element (RWST r w s m a) = a
type instance Element (Strict.RWST r w s m a) = a
type instance Element (ReaderT r m a) = a
type instance Element (ErrorT e m a) = a
type instance Element (ContT r m a) = a
type instance Element (Compose f g a) = a
type instance Element (Product f g a) = a
type instance Element (Static f a b) = b

class MonoFunctor c where
    cmap :: (Element c -> Element c) -> c -> c
    default cmap :: (Functor f, Element (f a) ~ a, f a ~ c) => (a -> a) -> f a -> f a
    cmap = fmap
instance MonoFunctor S.ByteString where
    cmap = S.map
instance MonoFunctor L.ByteString where
    cmap = L.map
instance MonoFunctor T.Text where
    cmap = T.map
instance MonoFunctor TL.Text where
    cmap = TL.map
instance MonoFunctor [a]
instance MonoFunctor (IO a)
instance MonoFunctor (ZipList a)
instance MonoFunctor (Maybe a)
instance MonoFunctor (Tree a)
instance MonoFunctor (Seq a)
instance MonoFunctor (ViewL a)
instance MonoFunctor (ViewR a)
instance MonoFunctor (IntMap a)
instance MonoFunctor (Option a)
instance MonoFunctor (NonEmpty a)
instance MonoFunctor (Identity a)
instance MonoFunctor (r -> a)
instance MonoFunctor (Either a b)
instance MonoFunctor (a, b)
instance MonoFunctor (Const m a)
instance Monad m => MonoFunctor (WrappedMonad m a)
instance MonoFunctor (Map k v)
instance MonoFunctor (HashMap k v)
instance MonoFunctor (Vector a)
instance Arrow a => MonoFunctor (WrappedArrow a b c)
instance Functor f => MonoFunctor (MaybeApply f a)
instance Functor f => MonoFunctor (WrappedApplicative f a)
instance MonoFunctor (Cokleisli w a b)
instance Functor m => MonoFunctor (MaybeT m a)
instance Functor m => MonoFunctor (ListT m a)
instance Functor m => MonoFunctor (IdentityT m a)
instance Functor m => MonoFunctor (WriterT w m a)
instance Functor m => MonoFunctor (Strict.WriterT w m a)
instance Functor m => MonoFunctor (StateT s m a)
instance Functor m => MonoFunctor (Strict.StateT s m a)
instance Functor m => MonoFunctor (RWST r w s m a)
instance Functor m => MonoFunctor (Strict.RWST r w s m a)
instance Functor m => MonoFunctor (ReaderT r m a)
instance Functor m => MonoFunctor (ErrorT e m a)
instance Functor m => MonoFunctor (ContT r m a)
instance (Functor f, Functor g) => MonoFunctor (Compose f g a)
instance (Functor f, Functor g) => MonoFunctor (Product f g a)
instance Functor f => MonoFunctor (Static f a b)

class MonoFoldable c where
    cfoldMap :: Monoid m => (Element c -> m) -> c -> m
    cfoldMap f = cfoldr (mappend . f) mempty

    cfoldr :: (Element c -> b -> b) -> b -> c -> b
    default cfoldr :: (t a ~ c, a ~ Element (t a), F.Foldable t) => (Element c -> b -> b) -> b -> c -> b
    cfoldr = F.foldr
    
    cfoldl' :: (a -> Element c -> a) -> a -> c -> a
    default cfoldl' :: (t b ~ c, b ~ Element (t b), F.Foldable t) => (a -> Element c -> a) -> a -> c -> a
    cfoldl' = F.foldl'

    ctoList :: c -> [Element c]
    ctoList t = build (\ c n -> cfoldr c n t)
    
    call :: (Element c -> Bool) -> c -> Bool
    call f = getAll . cfoldMap (All . f)
    
    cany :: (Element c -> Bool) -> c -> Bool
    cany f = getAny . cfoldMap (Any . f)
    
    cnull :: c -> Bool
    cnull = call (const False)

instance MonoFoldable S.ByteString where
    cfoldr = S.foldr
    cfoldl' = S.foldl'
    ctoList = S.unpack
    call = S.all
    cany = S.any
    cnull = S.null
instance MonoFoldable L.ByteString where
    cfoldr = L.foldr
    cfoldl' = L.foldl'
    ctoList = L.unpack
    call = L.all
    cany = L.any
    cnull = L.null
instance MonoFoldable T.Text where
    cfoldr = T.foldr
    cfoldl' = T.foldl'
    ctoList = T.unpack
    call = T.all
    cany = T.any
    cnull = T.null
instance MonoFoldable TL.Text where
    cfoldr = TL.foldr
    cfoldl' = TL.foldl'
    ctoList = TL.unpack
    call = TL.all
    cany = TL.any
    cnull = TL.null
instance MonoFoldable [a]
instance MonoFoldable (Maybe a)
instance MonoFoldable (Tree a)
instance MonoFoldable (Seq a)
instance MonoFoldable (ViewL a)
instance MonoFoldable (ViewR a)
instance MonoFoldable (IntMap a)
instance MonoFoldable (Option a)
instance MonoFoldable (NonEmpty a)
instance MonoFoldable (Identity a)
instance MonoFoldable (Map k v)
instance MonoFoldable (HashMap k v)
instance MonoFoldable (Vector a)

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
instance (MonoFoldable (t a), Monoid (t a)) => MonoFoldableMonoid (t a) -- FIXME
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
    default ctraverse :: (Traversable t, c ~ t a, a ~ Element c, Applicative f) => (Element c -> f (Element c)) -> c -> f c
    ctraverse = traverse
    cmapM :: Monad m => (Element c -> m (Element c)) -> c -> m c
    default cmapM :: (Traversable t, c ~ t a, a ~ Element c, Monad m) => (Element c -> m (Element c)) -> c -> m c
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
instance MonoTraversable [a]
instance MonoTraversable (Maybe a)
instance MonoTraversable (Tree a)
instance MonoTraversable (Seq a)
instance MonoTraversable (ViewL a)
instance MonoTraversable (ViewR a)
instance MonoTraversable (IntMap a)
instance MonoTraversable (Option a)
instance MonoTraversable (NonEmpty a)
instance MonoTraversable (Identity a)
instance MonoTraversable (Map k v)
instance MonoTraversable (HashMap k v)
instance MonoTraversable (Vector a)

cfor :: (MonoTraversable c, Applicative f) => c -> (Element c -> f (Element c)) -> f c
cfor = flip ctraverse

cforM :: (MonoTraversable c, Monad f) => c -> (Element c -> f (Element c)) -> f c
cforM = flip cmapM

class MonoPointed c where
    cpoint :: Element c -> c
instance (Pointed t, a ~ Element (t a)) => MonoPointed (t a) where -- FIXME remove, or perhaps all of MonoPointed
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
instance (Monoid (t a), Pointed t, a ~ Element (t a)) => FromList (t a)
instance FromList S.ByteString where
    fromList = S.pack
instance FromList L.ByteString where
    fromList = L.pack
instance FromList T.Text where
    fromList = T.pack
instance FromList TL.Text where
    fromList = TL.pack
