{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Type classes mirroring standard typeclasses, but working with monomorphic containers.
--
-- The motivation is that some commonly used data types (i.e., @ByteString@ and
-- @Text@) do not allow for instances of typeclasses like @Functor@ and
-- @Foldable@, since they are monomorphic structures. This module allows both
-- monomorphic and polymorphic data types to be instances of the same
-- typeclasses.
--
-- All of the laws for the polymorphic typeclasses apply to their monomorphic
-- cousins. Thus, even though a @MonoFunctor@ instance for @Set@ could
-- theoretically be defined, it is omitted since it could violate the functor
-- law of @omap f . omap g = omap (f . g)@.
--
-- Note that all typeclasses have been prefixed with @Mono@, and functions have
-- been prefixed with @o@. The mnemonic for @o@ is \"only one,\" or alternatively
-- \"it's mono, but m is overused in Haskell, so we'll use the second letter
-- instead.\" (Agreed, it's not a great mangling scheme, input is welcome!)
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
import           Prelude              (Bool (..), const, Char, flip, ($), IO, Maybe (..), Either (..),
                                       replicate, (+), Integral, Ordering (..), compare, fromIntegral, Num, (>=),
                                       seq, otherwise, maybe, Ord, (-))
import qualified Prelude
import qualified Data.ByteString.Internal as Unsafe
import qualified Foreign.ForeignPtr.Unsafe as Unsafe
import Foreign.Ptr (plusPtr)
import Foreign.ForeignPtr (touchForeignPtr)
import Foreign.Storable (peek)
import Control.Arrow (Arrow)
import Data.Tree (Tree)
import Data.Sequence (Seq, ViewL, ViewR)
import qualified Data.Sequence as Seq
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
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as VS
import qualified Data.IntSet as IntSet
import Data.Semigroup (Semigroup, Option (..))

type family Element mono
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
type instance Element (VS.Vector a) = a


class MonoFunctor mono where
    omap :: (Element mono -> Element mono) -> mono -> mono
    default omap :: (Functor f, Element (f a) ~ a, f a ~ mono) => (a -> a) -> f a -> f a
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
instance VS.Storable a => MonoFunctor (VS.Vector a) where
    omap = VS.map

class MonoFoldable mono where
    ofoldMap :: Monoid m => (Element mono -> m) -> mono -> m
    default ofoldMap :: (t a ~ mono, a ~ Element (t a), F.Foldable t, Monoid m) => (Element mono -> m) -> mono -> m
    ofoldMap = F.foldMap

    ofoldr :: (Element mono -> b -> b) -> b -> mono -> b
    default ofoldr :: (t a ~ mono, a ~ Element (t a), F.Foldable t) => (Element mono -> b -> b) -> b -> mono -> b
    ofoldr = F.foldr
    
    ofoldl' :: (a -> Element mono -> a) -> a -> mono -> a
    default ofoldl' :: (t b ~ mono, b ~ Element (t b), F.Foldable t) => (a -> Element mono -> a) -> a -> mono -> a
    ofoldl' = F.foldl'

    otoList :: mono -> [Element mono]
    otoList t = build (\ mono n -> ofoldr mono n t)
    
    oall :: (Element mono -> Bool) -> mono -> Bool
    oall f = getAll . ofoldMap (All . f)
    
    oany :: (Element mono -> Bool) -> mono -> Bool
    oany f = getAny . ofoldMap (Any . f)
    
    onull :: mono -> Bool
    onull = oall (const False)
    
    olength :: mono -> Int
    olength = ofoldl' (\i _ -> i + 1) 0
    
    olength64 :: mono -> Int64
    olength64 = ofoldl' (\i _ -> i + 1) 0
    
    ocompareLength :: Integral i => mono -> i -> Ordering
    ocompareLength c0 i0 = olength c0 `compare` fromIntegral i0 -- FIXME more efficient implementation

    otraverse_ :: (MonoFoldable mono, Applicative f) => (Element mono -> f b) -> mono -> f ()
    otraverse_ f = ofoldr ((*>) . f) (pure ())
    
    ofor_ :: (MonoFoldable mono, Applicative f) => mono -> (Element mono -> f b) -> f ()
    ofor_ = flip otraverse_
    
    omapM_ :: (MonoFoldable mono, Monad m) => (Element mono -> m b) -> mono -> m ()
    omapM_ f = ofoldr ((>>) . f) (return ())
    
    oforM_ :: (MonoFoldable mono, Monad m) => mono -> (Element mono -> m b) -> m ()
    oforM_ = flip omapM_
    {-# INLINE oforM_ #-}
    
    ofoldlM :: (MonoFoldable mono, Monad m) => (a -> Element mono -> m a) -> a -> mono -> m a
    ofoldlM f z0 xs = ofoldr f' return xs z0
      where f' x k z = f z x >>= k

    -- | Note: this is a partial function. On an empty @MonoFoldable@, it will
    -- throw an exception. See "Data.NonNull" for a total version of this
    -- function.
    ofoldMap1Ex :: Semigroup m => (Element mono -> m) -> mono -> m
    ofoldMap1Ex f = maybe (Prelude.error "Data.MonoTraversable.ofoldMap1Ex") id
                       . getOption . ofoldMap (Option . Just . f)

    -- | Note: this is a partial function. On an empty @MonoFoldable@, it will
    -- throw an exception. See "Data.NonNull" for a total version of this
    -- function.
    ofoldr1Ex :: (Element mono -> Element mono -> Element mono) -> mono -> Element mono
    default ofoldr1Ex :: (t a ~ mono, a ~ Element (t a), F.Foldable t)
                           => (a -> a -> a) -> mono -> a
    ofoldr1Ex = F.foldr1

    -- | Note: this is a partial function. On an empty @MonoFoldable@, it will
    -- throw an exception. See "Data.NonNull" for a total version of this
    -- function.
    ofoldl1Ex' :: (Element mono -> Element mono -> Element mono) -> mono -> Element mono
    default ofoldl1Ex' :: (t a ~ mono, a ~ Element (t a), F.Foldable t)
                            => (a -> a -> a) -> mono -> a
    ofoldl1Ex' = F.foldl1

    headEx :: mono -> Element mono
    headEx = ofoldr1Ex const

    lastEx :: mono -> Element mono
    lastEx = ofoldl1Ex' (flip const)

instance MonoFoldable S.ByteString where
    ofoldMap f = ofoldr (mappend . f) mempty
    ofoldr = S.foldr
    ofoldl' = S.foldl'
    otoList = S.unpack
    oall = S.all
    oany = S.any
    onull = S.null
    olength = S.length

    omapM_ f (Unsafe.PS fptr offset len) = do
        let start = Unsafe.unsafeForeignPtrToPtr fptr `plusPtr` offset
            end = start `plusPtr` len
            loop ptr
                | ptr >= end = Unsafe.inlinePerformIO (touchForeignPtr fptr) `seq` return ()
                | otherwise = do
                    _ <- f (Unsafe.inlinePerformIO (peek ptr))
                    loop (ptr `plusPtr` 1)
        loop start
    {-# INLINE omapM_ #-}
    ofoldr1Ex = S.foldr1
    ofoldl1Ex' = S.foldl1'
    headEx = S.head
    lastEx = S.last
instance MonoFoldable L.ByteString where
    ofoldMap f = ofoldr (mappend . f) mempty
    ofoldr = L.foldr
    ofoldl' = L.foldl'
    otoList = L.unpack
    oall = L.all
    oany = L.any
    onull = L.null
    olength64 = L.length
    omapM_ f = omapM_ (omapM_ f) . L.toChunks
    {-# INLINE omapM_ #-}
    ofoldr1Ex = L.foldr1
    ofoldl1Ex' = L.foldl1'
    headEx = L.head
    lastEx = L.last
instance MonoFoldable T.Text where
    ofoldMap f = ofoldr (mappend . f) mempty
    ofoldr = T.foldr
    ofoldl' = T.foldl'
    otoList = T.unpack
    oall = T.all
    oany = T.any
    onull = T.null
    olength = T.length
    ofoldr1Ex = T.foldr1
    ofoldl1Ex' = T.foldl1'
    headEx = T.head
    lastEx = T.last
instance MonoFoldable TL.Text where
    ofoldMap f = ofoldr (mappend . f) mempty
    ofoldr = TL.foldr
    ofoldl' = TL.foldl'
    otoList = TL.unpack
    oall = TL.all
    oany = TL.any
    onull = TL.null
    olength64 = TL.length
    ofoldr1Ex = TL.foldr1
    ofoldl1Ex' = TL.foldl1'
    headEx = TL.head
    lastEx = TL.last
instance MonoFoldable IntSet where
    ofoldMap f = ofoldr (mappend . f) mempty
    ofoldr = IntSet.foldr
    ofoldl' = IntSet.foldl'
    otoList = IntSet.toList
    onull = IntSet.null
    olength = IntSet.size
    ofoldr1Ex f = ofoldr1Ex f . IntSet.toList
    ofoldl1Ex' f = ofoldl1Ex' f . IntSet.toList
instance MonoFoldable [a] where
    otoList = id
    {-# INLINE otoList #-}
instance MonoFoldable (Maybe a)
instance MonoFoldable (Tree a)
instance MonoFoldable (Seq a) where
    headEx = flip Seq.index 1
    lastEx xs = Seq.index xs (Seq.length xs - 1)
instance MonoFoldable (ViewL a)
instance MonoFoldable (ViewR a)
instance MonoFoldable (IntMap a)
instance MonoFoldable (Option a)
instance MonoFoldable (NonEmpty a)
instance MonoFoldable (Identity a)
instance MonoFoldable (Map k v)
instance MonoFoldable (HashMap k v)
instance MonoFoldable (Vector a) where
    ofoldr = V.foldr
    ofoldl' = V.foldl'
    otoList = V.toList
    oall = V.all
    oany = V.any
    onull = V.null
    olength = V.length
    ofoldr1Ex = V.foldr1
    ofoldl1Ex' = V.foldl1'
    headEx = V.head
    lastEx = V.last
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
    ofoldr1Ex = U.foldr1
    ofoldl1Ex' = U.foldl1'
    headEx = U.head
    lastEx = U.last
instance VS.Storable a => MonoFoldable (VS.Vector a) where
    ofoldMap f = ofoldr (mappend . f) mempty
    ofoldr = VS.foldr
    ofoldl' = VS.foldl'
    otoList = VS.toList
    oall = VS.all
    oany = VS.any
    onull = VS.null
    olength = VS.length
    ofoldr1Ex = VS.foldr1
    ofoldl1Ex' = VS.foldl1'
    headEx = VS.head
    lastEx = VS.last
instance MonoFoldable (Either a b) where
    ofoldMap f = ofoldr (mappend . f) mempty
    ofoldr f b (Right a) = f a b
    ofoldr _ b (Left _) = b
    ofoldl' f a (Right b) = f a b
    ofoldl' _ a (Left _) = a
    otoList (Left _) = []
    otoList (Right b) = [b]
    oall _ (Left _) = True
    oall f (Right b) = f b
    oany _ (Left _) = False
    oany f (Right b) = f b
    onull (Left _) = True
    onull (Right _) = False
    olength (Left _) = 0
    olength (Right _) = 1
    ofoldr1Ex _ (Left _) = Prelude.error "ofoldr1Ex on Either"
    ofoldr1Ex _ (Right x) = x
    ofoldl1Ex' _ (Left _) = Prelude.error "ofoldl1Ex' on Either"
    ofoldl1Ex' _ (Right x) = x

-- | like Data.List.head, but not partial
headMay :: MonoFoldable mono => mono -> Maybe (Element mono)
headMay mono
    | onull mono = Nothing
    | otherwise = Just (headEx mono)

-- | like Data.List.last, but not partial
lastMay :: MonoFoldable mono => mono -> Maybe (Element mono)
lastMay mono
    | onull mono = Nothing
    | otherwise = Just (lastEx mono)

-- | The 'sum' function computes the sum of the numbers of a structure.
osum :: (MonoFoldable mono, Num (Element mono)) => mono -> Element mono
osum = getSum . ofoldMap Sum

-- | The 'product' function computes the product of the numbers of a structure.
oproduct :: (MonoFoldable mono, Num (Element mono)) => mono -> Element mono
oproduct = Data.Monoid.getProduct . ofoldMap Data.Monoid.Product

class (MonoFoldable mono, Monoid mono) => MonoFoldableMonoid mono where
    oconcatMap :: (Element mono -> mono) -> mono -> mono
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

-- | A typeclass for @MonoFoldable@s containing elements which are an instance
-- of @Ord@.
class (MonoFoldable mono, Ord (Element mono)) => MonoFoldableOrd mono where
    maximumEx :: mono -> Element mono
    maximumEx = maximumByEx compare

    maximumByEx :: (Element mono -> Element mono -> Ordering) -> mono -> Element mono
    maximumByEx f =
        ofoldl1Ex' go
      where
        go x y =
            case f x y of
                LT -> y
                _  -> x

    minimumEx :: mono -> Element mono
    minimumEx = minimumByEx compare

    minimumByEx :: (Element mono -> Element mono -> Ordering) -> mono -> Element mono
    minimumByEx f =
        ofoldl1Ex' go
      where
        go x y =
            case f x y of
                GT -> y
                _  -> x

instance MonoFoldableOrd S.ByteString where
    maximumEx = S.maximum
    {-# INLINE maximumEx #-}
    minimumEx = S.minimum
    {-# INLINE minimumEx #-}
instance MonoFoldableOrd L.ByteString where
    maximumEx = L.maximum
    {-# INLINE maximumEx #-}
    minimumEx = L.minimum
    {-# INLINE minimumEx #-}
instance MonoFoldableOrd T.Text where
    maximumEx = T.maximum
    {-# INLINE maximumEx #-}
    minimumEx = T.minimum
    {-# INLINE minimumEx #-}
instance MonoFoldableOrd TL.Text where
    maximumEx = TL.maximum
    {-# INLINE maximumEx #-}
    minimumEx = TL.minimum
    {-# INLINE minimumEx #-}
instance Ord a => MonoFoldableOrd IntSet
instance Ord a => MonoFoldableOrd [a]
instance Ord a => MonoFoldableOrd (Maybe a)
instance Ord a => MonoFoldableOrd (Tree a)
instance Ord a => MonoFoldableOrd (Seq a)
instance Ord a => MonoFoldableOrd (ViewL a)
instance Ord a => MonoFoldableOrd (ViewR a)
instance Ord a => MonoFoldableOrd (IntMap a)
instance Ord a => MonoFoldableOrd (Option a)
instance Ord a => MonoFoldableOrd (NonEmpty a)
instance Ord a => MonoFoldableOrd (Identity a)
instance Ord v => MonoFoldableOrd (Map k v)
instance Ord v => MonoFoldableOrd (HashMap k v)
instance Ord a => MonoFoldableOrd (Vector a) where
    maximumEx   = V.maximum
    maximumByEx = V.maximumBy
    minimumEx   = V.minimum
    minimumByEx = V.minimumBy
instance Ord e => MonoFoldableOrd (Set e)
instance Ord e => MonoFoldableOrd (HashSet e)
instance (U.Unbox a, Ord a) => MonoFoldableOrd (U.Vector a) where
    maximumEx   = U.maximum
    maximumByEx = U.maximumBy
    minimumEx   = U.minimum
    minimumByEx = U.minimumBy
instance (Ord a, VS.Storable a) => MonoFoldableOrd (VS.Vector a) where
    maximumEx   = VS.maximum
    maximumByEx = VS.maximumBy
    minimumEx   = VS.minimum
    minimumByEx = VS.minimumBy
instance Ord b => MonoFoldableOrd (Either a b) where

maximumMay :: MonoFoldableOrd mono => mono -> Maybe (Element mono)
maximumMay mono
    | onull mono = Nothing
    | otherwise = Just (maximumEx mono)

maximumByMay :: MonoFoldableOrd mono
             => (Element mono -> Element mono -> Ordering)
             -> mono
             -> Maybe (Element mono)
maximumByMay f mono
    | onull mono = Nothing
    | otherwise = Just (maximumByEx f mono)

minimumMay :: MonoFoldableOrd mono => mono -> Maybe (Element mono)
minimumMay mono
    | onull mono = Nothing
    | otherwise = Just (minimumEx mono)

minimumByMay :: MonoFoldableOrd mono
             => (Element mono -> Element mono -> Ordering)
             -> mono
             -> Maybe (Element mono)
minimumByMay f mono
    | onull mono = Nothing
    | otherwise = Just (minimumByEx f mono)

class (MonoFunctor mono, MonoFoldable mono) => MonoTraversable mono where
    otraverse :: Applicative f => (Element mono -> f (Element mono)) -> mono -> f mono
    default otraverse :: (Traversable t, mono ~ t a, a ~ Element mono, Applicative f) => (Element mono -> f (Element mono)) -> mono -> f mono
    otraverse = traverse
    omapM :: Monad m => (Element mono -> m (Element mono)) -> mono -> m mono
    default omapM :: (Traversable t, mono ~ t a, a ~ Element mono, Monad m) => (Element mono -> m (Element mono)) -> mono -> m mono
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
instance VS.Storable a => MonoTraversable (VS.Vector a) where
    otraverse f = fmap VS.fromList . traverse f . VS.toList
    omapM = VS.mapM
instance MonoTraversable (Either a b) where
    otraverse _ (Left a) = pure (Left a)
    otraverse f (Right b) = fmap Right (f b)
    omapM _ (Left a) = return (Left a)
    omapM f (Right b) = liftM Right (f b)

ofor :: (MonoTraversable mono, Applicative f) => mono -> (Element mono -> f (Element mono)) -> f mono
ofor = flip otraverse

oforM :: (MonoTraversable mono, Monad f) => mono -> (Element mono -> f (Element mono)) -> f mono
oforM = flip omapM
