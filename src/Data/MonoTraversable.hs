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
import           Data.Monoid (Monoid (..), Any (..), All (..), Sum (..))
import qualified Data.Monoid
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL
import           Data.Traversable
import           Data.Word            (Word8)
import Data.Int (Int, Int64)
import           GHC.Exts             (build)
import           Prelude              (Bool (..), const, Char, flip, ($), IO, Maybe, Either,
                                       replicate, (+), Integral, Ordering (..), compare, fromIntegral, Num)
import Control.Arrow (Arrow)
import Data.Tree (Tree)
import Data.Sequence (Seq, ViewL, ViewR)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
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
import Data.Set (Set)
import Data.HashSet (HashSet)
import qualified Data.Vector.Unboxed as U
import qualified Data.IntSet as IntSet

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
type instance Element IntSet = Int
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
type instance Element (Set e) = e
type instance Element (HashSet e) = e
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
type instance Element (U.Vector a) = a

class MonoFunctor c where
    omap :: (Element c -> Element c) -> c -> c
    default omap :: (Functor f, Element (f a) ~ a, f a ~ c) => (a -> a) -> f a -> f a
    omap = fmap
instance MonoFunctor S.ByteString where
    omap = S.map
instance MonoFunctor L.ByteString where
    omap = L.map
instance MonoFunctor T.Text where
    omap = T.map
instance MonoFunctor TL.Text where
    omap = TL.map
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
instance U.Unbox a => MonoFunctor (U.Vector a) where
    omap = U.map

class MonoFoldable c where
    ofoldMap :: Monoid m => (Element c -> m) -> c -> m
    default ofoldMap :: (t a ~ c, a ~ Element (t a), F.Foldable t, Monoid m) => (Element c -> m) -> c -> m
    ofoldMap = F.foldMap

    ofoldr :: (Element c -> b -> b) -> b -> c -> b
    default ofoldr :: (t a ~ c, a ~ Element (t a), F.Foldable t) => (Element c -> b -> b) -> b -> c -> b
    ofoldr = F.foldr
    
    ofoldl' :: (a -> Element c -> a) -> a -> c -> a
    default ofoldl' :: (t b ~ c, b ~ Element (t b), F.Foldable t) => (a -> Element c -> a) -> a -> c -> a
    ofoldl' = F.foldl'

    otoList :: c -> [Element c]
    otoList t = build (\ c n -> ofoldr c n t)
    
    oall :: (Element c -> Bool) -> c -> Bool
    oall f = getAll . ofoldMap (All . f)
    
    oany :: (Element c -> Bool) -> c -> Bool
    oany f = getAny . ofoldMap (Any . f)
    
    onull :: c -> Bool
    onull = oall (const False)
    
    olength :: c -> Int
    olength = ofoldl' (\i _ -> i + 1) 0
    
    olength64 :: c -> Int64
    olength64 = ofoldl' (\i _ -> i + 1) 0
    
    ocompareLength :: Integral i => c -> i -> Ordering
    ocompareLength c0 i0 = olength c0 `compare` fromIntegral i0 -- FIXME more efficient implementation

    otraverse_ :: (MonoFoldable c, Applicative f) => (Element c -> f b) -> c -> f ()
    otraverse_ f = ofoldr ((*>) . f) (pure ())
    
    ofor_ :: (MonoFoldable c, Applicative f) => c -> (Element c -> f b) -> f ()
    ofor_ = flip otraverse_
    
    omapM_ :: (MonoFoldable c, Monad m) => (Element c -> m b) -> c -> m ()
    omapM_ f = ofoldr ((>>) . f) (return ())
    
    oforM_ :: (MonoFoldable c, Monad m) => c -> (Element c -> m b) -> m ()
    oforM_ = flip omapM_
    
    ofoldlM :: (MonoFoldable c, Monad m) => (a -> Element c -> m a) -> a -> c -> m a
    ofoldlM f z0 xs = ofoldr f' return xs z0
      where f' x k z = f z x >>= k
    
instance MonoFoldable S.ByteString where
    ofoldMap f = ofoldr (mappend . f) mempty
    ofoldr = S.foldr
    ofoldl' = S.foldl'
    otoList = S.unpack
    oall = S.all
    oany = S.any
    onull = S.null
    olength = S.length
instance MonoFoldable L.ByteString where
    ofoldMap f = ofoldr (mappend . f) mempty
    ofoldr = L.foldr
    ofoldl' = L.foldl'
    otoList = L.unpack
    oall = L.all
    oany = L.any
    onull = L.null
    olength64 = L.length
instance MonoFoldable T.Text where
    ofoldMap f = ofoldr (mappend . f) mempty
    ofoldr = T.foldr
    ofoldl' = T.foldl'
    otoList = T.unpack
    oall = T.all
    oany = T.any
    onull = T.null
    olength = T.length
instance MonoFoldable TL.Text where
    ofoldMap f = ofoldr (mappend . f) mempty
    ofoldr = TL.foldr
    ofoldl' = TL.foldl'
    otoList = TL.unpack
    oall = TL.all
    oany = TL.any
    onull = TL.null
    olength64 = TL.length
instance MonoFoldable IntSet where
    ofoldMap f = ofoldr (mappend . f) mempty
    ofoldr = IntSet.foldr
    ofoldl' = IntSet.foldl'
    otoList = IntSet.toList
    onull = IntSet.null
    olength = IntSet.size
instance MonoFoldable [a] where
    otoList = id
    {-# INLINE otoList #-}
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
instance MonoFoldable (Set e)
instance MonoFoldable (HashSet e)
instance U.Unbox a => MonoFoldable (U.Vector a) where
    ofoldMap f = ofoldr (mappend . f) mempty
    ofoldr = U.foldr
    ofoldl' = U.foldl'
    otoList = U.toList
    oall = U.all
    oany = U.any
    onull = U.null
    olength = U.length

-- | The 'sum' function computes the sum of the numbers of a structure.
osum :: (MonoFoldable c, Num (Element c)) => c -> Element c
osum = getSum . ofoldMap Sum

-- | The 'product' function computes the product of the numbers of a structure.
oproduct :: (MonoFoldable c, Num (Element c)) => c -> Element c
oproduct = Data.Monoid.getProduct . ofoldMap Data.Monoid.Product

class (MonoFoldable c, Monoid c) => MonoFoldableMonoid c where
    oconcatMap :: (Element c -> c) -> c -> c
    oconcatMap = ofoldMap
instance (MonoFoldable (t a), Monoid (t a)) => MonoFoldableMonoid (t a) -- FIXME
instance MonoFoldableMonoid S.ByteString where
    oconcatMap = S.concatMap
instance MonoFoldableMonoid L.ByteString where
    oconcatMap = L.concatMap
instance MonoFoldableMonoid T.Text where
    oconcatMap = T.concatMap
instance MonoFoldableMonoid TL.Text where
    oconcatMap = TL.concatMap

class (MonoFunctor c, MonoFoldable c) => MonoTraversable c where
    otraverse :: Applicative f => (Element c -> f (Element c)) -> c -> f c
    default otraverse :: (Traversable t, c ~ t a, a ~ Element c, Applicative f) => (Element c -> f (Element c)) -> c -> f c
    otraverse = traverse
    omapM :: Monad m => (Element c -> m (Element c)) -> c -> m c
    default omapM :: (Traversable t, c ~ t a, a ~ Element c, Monad m) => (Element c -> m (Element c)) -> c -> m c
    omapM = mapM
instance MonoTraversable S.ByteString where
    otraverse f = fmap S.pack . traverse f . S.unpack
    omapM f = liftM S.pack . mapM f . S.unpack
instance MonoTraversable L.ByteString where
    otraverse f = fmap L.pack . traverse f . L.unpack
    omapM f = liftM L.pack . mapM f . L.unpack
instance MonoTraversable T.Text where
    otraverse f = fmap T.pack . traverse f . T.unpack
    omapM f = liftM T.pack . mapM f . T.unpack
instance MonoTraversable TL.Text where
    otraverse f = fmap TL.pack . traverse f . TL.unpack
    omapM f = liftM TL.pack . mapM f . TL.unpack
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
instance U.Unbox a => MonoTraversable (U.Vector a) where
    otraverse f = fmap U.fromList . traverse f . U.toList
    omapM = U.mapM

ofor :: (MonoTraversable c, Applicative f) => c -> (Element c -> f (Element c)) -> f c
ofor = flip otraverse

oforM :: (MonoTraversable c, Monad f) => c -> (Element c -> f (Element c)) -> f c
oforM = flip omapM
