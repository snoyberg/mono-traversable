{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DefaultSignatures       #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
-- | Type classes mirroring standard typeclasses, but working with monomorphic containers.
--
-- The motivation is that some commonly used data types (i.e., 'ByteString' and
-- 'Text') do not allow for instances of typeclasses like 'Functor' and
-- 'Foldable', since they are monomorphic structures. This module allows both
-- monomorphic and polymorphic data types to be instances of the same
-- typeclasses.
--
-- All of the laws for the polymorphic typeclasses apply to their monomorphic
-- cousins. Thus, even though a 'MonoFunctor' instance for 'Set' could
-- theoretically be defined, it is omitted since it could violate the functor
-- law of @'omap' f . 'omap' g = 'omap' (f . g)@.
--
-- Note that all typeclasses have been prefixed with @Mono@, and functions have
-- been prefixed with @o@. The mnemonic for @o@ is "only one", or alternatively
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
import           Data.Maybe           (fromMaybe)
import           Data.Monoid (Monoid (..), Any (..), All (..))
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL
import           Data.Traversable
import           Data.Traversable.Instances ()
import           Data.Word            (Word8)
import Data.Int (Int, Int64)
import           GHC.Exts             (build)
import           Prelude              (Bool (..), const, Char, flip, IO, Maybe (..), Either (..),
                                       (+), Integral, Ordering (..), compare, fromIntegral, Num, (>=),
                                       seq, otherwise, Eq, Ord, (-), (*))
import qualified Prelude
import qualified Data.ByteString.Internal as Unsafe
import qualified Foreign.ForeignPtr.Unsafe as Unsafe
import Foreign.Ptr (plusPtr)
import Foreign.ForeignPtr (touchForeignPtr)
import Foreign.Storable (peek)
import Control.Arrow (Arrow)
import Data.Tree (Tree (..))
import Data.Sequence (Seq, ViewL (..), ViewR (..))
import qualified Data.Sequence as Seq
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import Data.Functor.Identity (Identity)
import Data.Map (Map)
import Data.HashMap.Strict (HashMap)
import Data.Vector (Vector)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.List (ListT)
import Control.Monad.Trans.Identity (IdentityT)
import Data.Functor.Apply (MaybeApply (..), WrappedApplicative)
import Control.Comonad (Cokleisli, Comonad, extract, extend)
import Control.Comonad.Store (StoreT)
import Control.Comonad.Env (EnvT)
import Control.Comonad.Traced (TracedT)
import Data.Functor.Coproduct (Coproduct)
import Control.Monad.Trans.Writer (WriterT)
import qualified Control.Monad.Trans.Writer.Strict as Strict (WriterT)
import Control.Monad.Trans.State (StateT(..))
import qualified Control.Monad.Trans.State.Strict as Strict (StateT(..))
import Control.Monad.Trans.RWS (RWST(..))
import qualified Control.Monad.Trans.RWS.Strict as Strict (RWST(..))
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Error (ErrorT(..))
import Control.Monad.Trans.Cont (ContT)
import Data.Functor.Compose (Compose)
import Data.Functor.Product (Product)
import Data.Semigroupoid.Static (Static)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as VS
import qualified Data.IntSet as IntSet
import Data.Semigroup (Semigroup, Option (..), Arg)
import qualified Data.ByteString.Unsafe as SU
import Data.DList (DList)
import qualified Data.DList as DL

-- | Type family for getting the type of the elements
-- of a monomorphic container.
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
type instance Element (DList a) = a
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
type instance Element (Arg a b) = b
type instance Element (EnvT e w a) = a
type instance Element (StoreT s w a) = a
type instance Element (TracedT m w a) = a
type instance Element (Coproduct f g a) = a

-- | Monomorphic containers that can be mapped over.
class MonoFunctor mono where
    -- | Map over a monomorphic container
    omap :: (Element mono -> Element mono) -> mono -> mono
    default omap :: (Functor f, Element (f a) ~ a, f a ~ mono) => (a -> a) -> f a -> f a
    omap = fmap
    {-# INLINE omap #-}

instance MonoFunctor S.ByteString where
    omap = S.map
    {-# INLINE omap #-}
instance MonoFunctor L.ByteString where
    omap = L.map
    {-# INLINE omap #-}
instance MonoFunctor T.Text where
    omap = T.map
    {-# INLINE omap #-}
instance MonoFunctor TL.Text where
    omap = TL.map
    {-# INLINE omap #-}
instance MonoFunctor [a]
instance MonoFunctor (IO a)
instance MonoFunctor (ZipList a)
instance MonoFunctor (Maybe a)
instance MonoFunctor (Tree a)
instance MonoFunctor (Seq a)
instance MonoFunctor (DList a)
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
instance MonoFunctor (Arg a b)
instance Functor w => MonoFunctor (EnvT e w a)
instance Functor w => MonoFunctor (StoreT s w a)
instance Functor w => MonoFunctor (TracedT m w a)
instance (Functor f, Functor g) => MonoFunctor (Coproduct f g a)
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
    {-# INLINE omap #-}
instance VS.Storable a => MonoFunctor (VS.Vector a) where
    omap = VS.map
    {-# INLINE omap #-}

-- | Monomorphic Applicatives.

class MonoApplicative mono where
    -- | Pure for monomorphic container.
    opure :: a -> mono a
    default opure :: (Applicative f, Element (f a) ~ a, f a ~ mono a) => a -> f a
    opure = pure
    {-# INLINE opure #-}

    -- | Apply over a monomorphic container.
    oap :: mono (a -> a) -> mono a -> mono a
    default oap :: (Applicative f, Element (f a) ~ a, f a ~ mono a) => f (a -> a) -> f a -> f a
    oap = (<*>)
    {-# INLINE oap #-}

instance MonoApplicative []
instance MonoApplicative IO
instance MonoApplicative Maybe
instance MonoApplicative ZipList
instance MonoApplicative Identity
instance MonoApplicative ((->) a)
instance MonoApplicative (Either e)
instance Monoid a => MonoApplicative ((,) a)
instance Monad m => MonoApplicative (WrappedMonad m)
instance Monoid m => MonoApplicative (Const m)
instance Arrow a => MonoApplicative (WrappedArrow a b)

-- | Monomorphic containers that can be folded.
class MonoFoldable mono where
    -- | Map each element of a monomorphic container to a 'Monoid'
    -- and combine the results.
    ofoldMap :: Monoid m => (Element mono -> m) -> mono -> m
    default ofoldMap :: (t a ~ mono, a ~ Element (t a), F.Foldable t, Monoid m) => (Element mono -> m) -> mono -> m
    ofoldMap = F.foldMap
    {-# INLINE ofoldMap #-}

    -- | Right-associative fold of a monomorphic container.
    ofoldr :: (Element mono -> b -> b) -> b -> mono -> b
    default ofoldr :: (t a ~ mono, a ~ Element (t a), F.Foldable t) => (Element mono -> b -> b) -> b -> mono -> b
    ofoldr = F.foldr
    {-# INLINE ofoldr #-}

    -- | Strict left-associative fold of a monomorphic container.
    ofoldl' :: (a -> Element mono -> a) -> a -> mono -> a
    default ofoldl' :: (t b ~ mono, b ~ Element (t b), F.Foldable t) => (a -> Element mono -> a) -> a -> mono -> a
    ofoldl' = F.foldl'
    {-# INLINE ofoldl' #-}

    -- | Convert a monomorphic container to a list.
    otoList :: mono -> [Element mono]
    otoList t = build (\ mono n -> ofoldr mono n t)
    {-# INLINE otoList #-}

    -- | Are __all__ of the elements in a monomorphic container
    -- converted to booleans 'True'?
    oall :: (Element mono -> Bool) -> mono -> Bool
    oall f = getAll . ofoldMap (All . f)
    {-# INLINE oall #-}

    -- | Are __any__ of the elements in a monomorphic container
    -- converted to booleans 'True'?
    oany :: (Element mono -> Bool) -> mono -> Bool
    oany f = getAny . ofoldMap (Any . f)
    {-# INLINE oany #-}

    -- | Is the monomorphic container empty?
    onull :: mono -> Bool
    onull = oall (const False)
    {-# INLINE onull #-}

    -- | Length of a monomorphic container, returns a 'Int'.
    olength :: mono -> Int
    olength = ofoldl' (\i _ -> i + 1) 0
    {-# INLINE olength #-}

    -- | Length of a monomorphic container, returns a 'Int64'.
    olength64 :: mono -> Int64
    olength64 = ofoldl' (\i _ -> i + 1) 0
    {-# INLINE olength64 #-}

    -- | Compare the length of a monomorphic container and a given number.
    ocompareLength :: Integral i => mono -> i -> Ordering
    -- Basic implementation using length for most instance. See the list
    -- instance below for support for infinite structures. Arguably, that
    -- should be the default instead of this.
    ocompareLength c0 i0 = olength c0 `compare` fromIntegral i0
    {-# INLINE ocompareLength #-}

    -- | Map each element of a monomorphic container to an action,
    -- evaluate these actions from left to right, and ignore the results.
    otraverse_ :: (MonoFoldable mono, Applicative f) => (Element mono -> f b) -> mono -> f ()
    otraverse_ f = ofoldr ((*>) . f) (pure ())
    {-# INLINE otraverse_ #-}

    -- | 'ofor_' is 'otraverse_' with its arguments flipped.
    ofor_ :: (MonoFoldable mono, Applicative f) => mono -> (Element mono -> f b) -> f ()
    ofor_ = flip otraverse_
    {-# INLINE ofor_ #-}

    -- | Map each element of a monomorphic container to a monadic action,
    -- evaluate these actions from left to right, and ignore the results.
    omapM_ :: (MonoFoldable mono, Monad m) => (Element mono -> m ()) -> mono -> m ()
    omapM_ f = ofoldr ((>>) . f) (return ())
    {-# INLINE omapM_ #-}

    -- | 'oforM_' is 'omapM_' with its arguments flipped.
    oforM_ :: (MonoFoldable mono, Monad m) => mono -> (Element mono -> m ()) -> m ()
    oforM_ = flip omapM_
    {-# INLINE oforM_ #-}

    -- | Monadic fold over the elements of a monomorphic container, associating to the left.
    ofoldlM :: (MonoFoldable mono, Monad m) => (a -> Element mono -> m a) -> a -> mono -> m a
    ofoldlM f z0 xs = ofoldr f' return xs z0
      where f' x k z = f z x >>= k
    {-# INLINE ofoldlM #-}

    -- | Map each element of a monomorphic container to a semigroup,
    -- and combine the results.
    --
    -- Note: this is a partial function. On an empty 'MonoFoldable', it will
    -- throw an exception.
    --
    -- /See 'Data.MinLen.ofoldMap1' from "Data.MinLen" for a total version of this function./
    ofoldMap1Ex :: Semigroup m => (Element mono -> m) -> mono -> m
    ofoldMap1Ex f = fromMaybe (Prelude.error "Data.MonoTraversable.ofoldMap1Ex")
                       . getOption . ofoldMap (Option . Just . f)

    -- | Right-associative fold of a monomorphic container with no base element.
    --
    -- Note: this is a partial function. On an empty 'MonoFoldable', it will
    -- throw an exception.
    --
    -- /See 'Data.MinLen.ofoldr1Ex' from "Data.MinLen" for a total version of this function./
    ofoldr1Ex :: (Element mono -> Element mono -> Element mono) -> mono -> Element mono
    default ofoldr1Ex :: (t a ~ mono, a ~ Element (t a), F.Foldable t)
                           => (a -> a -> a) -> mono -> a
    ofoldr1Ex = F.foldr1
    {-# INLINE ofoldr1Ex #-}

    -- | Strict left-associative fold of a monomorphic container with no base
    -- element.
    --
    -- Note: this is a partial function. On an empty 'MonoFoldable', it will
    -- throw an exception.
    --
    -- /See 'Data.MinLen.ofoldl1Ex'' from "Data.MinLen" for a total version of this function./
    ofoldl1Ex' :: (Element mono -> Element mono -> Element mono) -> mono -> Element mono
    default ofoldl1Ex' :: (t a ~ mono, a ~ Element (t a), F.Foldable t)
                            => (a -> a -> a) -> mono -> a
    ofoldl1Ex' = F.foldl1
    {-# INLINE ofoldl1Ex' #-}

    -- | Get the first element of a monomorphic container.
    --
    -- Note: this is a partial function. On an empty 'MonoFoldable', it will
    -- throw an exception.
    --
    -- /See 'Data.MinLen.head' from "Data.MinLen" for a total version of this function./
    headEx :: mono -> Element mono
    headEx = ofoldr const (Prelude.error "Data.MonoTraversable.headEx: empty")
    {-# INLINE headEx #-}

    -- | Get the last element of a monomorphic container.
    --
    -- Note: this is a partial function. On an empty 'MonoFoldable', it will
    -- throw an exception.
    --
    -- /See 'Data.MinLen.last from "Data.MinLen" for a total version of this function./
    lastEx :: mono -> Element mono
    lastEx = ofoldl1Ex' (flip const)
    {-# INLINE lastEx #-}

    -- | Equivalent to 'headEx'.
    unsafeHead :: mono -> Element mono
    unsafeHead = headEx
    {-# INLINE unsafeHead #-}

    -- | Equivalent to 'lastEx'.
    unsafeLast :: mono -> Element mono
    unsafeLast = lastEx
    {-# INLINE unsafeLast #-}

    -- | Get the maximum element of a monomorphic container,
    -- using a supplied element ordering function.
    --
    -- Note: this is a partial function. On an empty 'MonoFoldable', it will
    -- throw an exception.
    --
    -- /See 'Data.MinLen.maximiumBy' from "Data.MinLen" for a total version of this function./
    maximumByEx :: (Element mono -> Element mono -> Ordering) -> mono -> Element mono
    maximumByEx f =
        ofoldl1Ex' go
      where
        go x y =
            case f x y of
                LT -> y
                _  -> x
    {-# INLINE maximumByEx #-}

    -- | Get the minimum element of a monomorphic container,
    -- using a supplied element ordering function.
    --
    -- Note: this is a partial function. On an empty 'MonoFoldable', it will
    -- throw an exception.
    --
    -- /See 'Data.MinLen.minimumBy' from "Data.MinLen" for a total version of this function./
    minimumByEx :: (Element mono -> Element mono -> Ordering) -> mono -> Element mono
    minimumByEx f =
        ofoldl1Ex' go
      where
        go x y =
            case f x y of
                GT -> y
                _  -> x
    {-# INLINE minimumByEx #-}

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
    ofoldr1Ex = S.foldr1
    ofoldl1Ex' = S.foldl1'
    headEx = S.head
    lastEx = S.last
    unsafeHead = SU.unsafeHead
    {-# INLINE ofoldMap #-}
    {-# INLINE ofoldr #-}
    {-# INLINE ofoldl' #-}
    {-# INLINE otoList #-}
    {-# INLINE oall #-}
    {-# INLINE oany #-}
    {-# INLINE onull #-}
    {-# INLINE olength #-}
    {-# INLINE omapM_ #-}
    {-# INLINE ofoldr1Ex #-}
    {-# INLINE ofoldl1Ex' #-}
    {-# INLINE headEx #-}
    {-# INLINE lastEx #-}
    {-# INLINE unsafeHead #-}
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
    ofoldr1Ex = L.foldr1
    ofoldl1Ex' = L.foldl1'
    headEx = L.head
    lastEx = L.last
    {-# INLINE ofoldMap #-}
    {-# INLINE ofoldr #-}
    {-# INLINE ofoldl' #-}
    {-# INLINE otoList #-}
    {-# INLINE oall #-}
    {-# INLINE oany #-}
    {-# INLINE onull #-}
    {-# INLINE olength64 #-}
    {-# INLINE omapM_ #-}
    {-# INLINE ofoldr1Ex #-}
    {-# INLINE ofoldl1Ex' #-}
    {-# INLINE headEx #-}
    {-# INLINE lastEx #-}
    {-# INLINE unsafeHead #-}
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
    {-# INLINE ofoldMap #-}
    {-# INLINE ofoldr #-}
    {-# INLINE ofoldl' #-}
    {-# INLINE otoList #-}
    {-# INLINE oall #-}
    {-# INLINE oany #-}
    {-# INLINE onull #-}
    {-# INLINE olength #-}
    {-# INLINE omapM_ #-}
    {-# INLINE ofoldr1Ex #-}
    {-# INLINE ofoldl1Ex' #-}
    {-# INLINE headEx #-}
    {-# INLINE lastEx #-}
    {-# INLINE unsafeHead #-}
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
    {-# INLINE ofoldMap #-}
    {-# INLINE ofoldr #-}
    {-# INLINE ofoldl' #-}
    {-# INLINE otoList #-}
    {-# INLINE oall #-}
    {-# INLINE oany #-}
    {-# INLINE onull #-}
    {-# INLINE olength #-}
    {-# INLINE omapM_ #-}
    {-# INLINE ofoldr1Ex #-}
    {-# INLINE ofoldl1Ex' #-}
    {-# INLINE headEx #-}
    {-# INLINE lastEx #-}
    {-# INLINE unsafeHead #-}
instance MonoFoldable IntSet where
    ofoldMap f = ofoldr (mappend . f) mempty
    ofoldr = IntSet.foldr
    ofoldl' = IntSet.foldl'
    otoList = IntSet.toList
    onull = IntSet.null
    olength = IntSet.size
    ofoldr1Ex f = ofoldr1Ex f . IntSet.toList
    ofoldl1Ex' f = ofoldl1Ex' f . IntSet.toList
    {-# INLINE ofoldMap #-}
    {-# INLINE ofoldr #-}
    {-# INLINE ofoldl' #-}
    {-# INLINE otoList #-}
    {-# INLINE oall #-}
    {-# INLINE oany #-}
    {-# INLINE onull #-}
    {-# INLINE olength #-}
    {-# INLINE omapM_ #-}
    {-# INLINE ofoldr1Ex #-}
    {-# INLINE ofoldl1Ex' #-}
    {-# INLINE headEx #-}
    {-# INLINE lastEx #-}
    {-# INLINE unsafeHead #-}
instance MonoFoldable [a] where
    otoList = id
    {-# INLINE otoList #-}

    ocompareLength [] i = 0 `compare` i
    ocompareLength (_:xs) i
        | i Prelude.<= 0 = GT
        | otherwise = ocompareLength xs (i - 1)
instance MonoFoldable (Maybe a) where
    omapM_ _ Nothing = return ()
    omapM_ f (Just x) = f x
    {-# INLINE omapM_ #-}
instance MonoFoldable (Tree a)
instance MonoFoldable (Seq a) where
    headEx = flip Seq.index 0
    lastEx xs = Seq.index xs (Seq.length xs - 1)
    {-# INLINE headEx #-}
    {-# INLINE lastEx #-}
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
    unsafeHead = V.unsafeHead
    unsafeLast = V.unsafeLast
    maximumByEx = V.maximumBy
    minimumByEx = V.minimumBy
    {-# INLINE ofoldr #-}
    {-# INLINE ofoldl' #-}
    {-# INLINE otoList #-}
    {-# INLINE oall #-}
    {-# INLINE oany #-}
    {-# INLINE onull #-}
    {-# INLINE olength #-}
    {-# INLINE omapM_ #-}
    {-# INLINE ofoldr1Ex #-}
    {-# INLINE ofoldl1Ex' #-}
    {-# INLINE headEx #-}
    {-# INLINE lastEx #-}
    {-# INLINE unsafeHead #-}
    {-# INLINE maximumByEx #-}
    {-# INLINE minimumByEx #-}
instance MonoFoldable (Set e)
instance MonoFoldable (HashSet e)
instance MonoFoldable (DList a) where
    otoList = DL.toList
    headEx = DL.head
    {-# INLINE otoList #-}
    {-# INLINE headEx #-}

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
    unsafeHead = U.unsafeHead
    unsafeLast = U.unsafeLast
    maximumByEx = U.maximumBy
    minimumByEx = U.minimumBy
    {-# INLINE ofoldMap #-}
    {-# INLINE ofoldr #-}
    {-# INLINE ofoldl' #-}
    {-# INLINE otoList #-}
    {-# INLINE oall #-}
    {-# INLINE oany #-}
    {-# INLINE onull #-}
    {-# INLINE olength #-}
    {-# INLINE omapM_ #-}
    {-# INLINE ofoldr1Ex #-}
    {-# INLINE ofoldl1Ex' #-}
    {-# INLINE headEx #-}
    {-# INLINE lastEx #-}
    {-# INLINE unsafeHead #-}
    {-# INLINE maximumByEx #-}
    {-# INLINE minimumByEx #-}
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
    unsafeHead = VS.unsafeHead
    unsafeLast = VS.unsafeLast
    maximumByEx = VS.maximumBy
    minimumByEx = VS.minimumBy
    {-# INLINE ofoldMap #-}
    {-# INLINE ofoldr #-}
    {-# INLINE ofoldl' #-}
    {-# INLINE otoList #-}
    {-# INLINE oall #-}
    {-# INLINE oany #-}
    {-# INLINE onull #-}
    {-# INLINE olength #-}
    {-# INLINE omapM_ #-}
    {-# INLINE ofoldr1Ex #-}
    {-# INLINE ofoldl1Ex' #-}
    {-# INLINE headEx #-}
    {-# INLINE lastEx #-}
    {-# INLINE unsafeHead #-}
    {-# INLINE maximumByEx #-}
    {-# INLINE minimumByEx #-}
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
    omapM_ _ (Left _) = return ()
    omapM_ f (Right x) = f x
    {-# INLINE ofoldMap #-}
    {-# INLINE ofoldr #-}
    {-# INLINE ofoldl' #-}
    {-# INLINE otoList #-}
    {-# INLINE oall #-}
    {-# INLINE oany #-}
    {-# INLINE onull #-}
    {-# INLINE olength #-}
    {-# INLINE omapM_ #-}
    {-# INLINE ofoldr1Ex #-}
    {-# INLINE ofoldl1Ex' #-}
    {-# INLINE headEx #-}
    {-# INLINE lastEx #-}
    {-# INLINE unsafeHead #-}
instance MonoFoldable (a, b)
instance MonoFoldable (Const m a)
instance F.Foldable f => MonoFoldable (MaybeT f a)
instance F.Foldable f => MonoFoldable (ListT f a)
instance F.Foldable f => MonoFoldable (IdentityT f a)
instance F.Foldable f => MonoFoldable (WriterT w f a)
instance F.Foldable f => MonoFoldable (Strict.WriterT w f a)
instance F.Foldable f => MonoFoldable (ErrorT e f a)
instance (F.Foldable f, F.Foldable g) => MonoFoldable (Compose f g a)
instance (F.Foldable f, F.Foldable g) => MonoFoldable (Product f g a)

-- | Safe version of 'headEx'.
--
-- Returns 'Nothing' instead of throwing an exception when encountering
-- an empty monomorphic container.
headMay :: MonoFoldable mono => mono -> Maybe (Element mono)
headMay mono
    | onull mono = Nothing
    | otherwise = Just (headEx mono)
{-# INLINE headMay #-}

-- | Safe version of 'lastEx'.
--
-- Returns 'Nothing' instead of throwing an exception when encountering
-- an empty monomorphic container.
lastMay :: MonoFoldable mono => mono -> Maybe (Element mono)
lastMay mono
    | onull mono = Nothing
    | otherwise = Just (lastEx mono)
{-# INLINE lastMay #-}

-- | 'osum' computes the sum of the numbers of a monomorphic container.
osum :: (MonoFoldable mono, Num (Element mono)) => mono -> Element mono
osum = ofoldl' (+) 0
{-# INLINE osum #-}

-- | 'oproduct' computes the product of the numbers of a monomorphic container.
oproduct :: (MonoFoldable mono, Num (Element mono)) => mono -> Element mono
oproduct = ofoldl' (*) 1
{-# INLINE oproduct #-}

-- | Are __all__ of the elements 'True'?
--
-- Since 0.6.0
oand :: (Element mono ~ Bool, MonoFoldable mono) => mono -> Bool
oand = oall id
{-# INLINE oand #-}

-- | Are __any__ of the elements 'True'?
--
-- Since 0.6.0
oor :: (Element mono ~ Bool, MonoFoldable mono) => mono -> Bool
oor = oany id
{-# INLINE oor #-}

-- | A typeclass for monomorphic containers that are 'Monoid's.
class (MonoFoldable mono, Monoid mono) => MonoFoldableMonoid mono where -- FIXME is this really just MonoMonad?
    -- | Map a function over a monomorphic container and combine the results.
    oconcatMap :: (Element mono -> mono) -> mono -> mono
    oconcatMap = ofoldMap
    {-# INLINE oconcatMap #-}
instance (MonoFoldable (t a), Monoid (t a)) => MonoFoldableMonoid (t a) -- FIXME
instance MonoFoldableMonoid S.ByteString where
    oconcatMap = S.concatMap
    {-# INLINE oconcatMap #-}
instance MonoFoldableMonoid L.ByteString where
    oconcatMap = L.concatMap
    {-# INLINE oconcatMap #-}
instance MonoFoldableMonoid T.Text where
    oconcatMap = T.concatMap
    {-# INLINE oconcatMap #-}
instance MonoFoldableMonoid TL.Text where
    oconcatMap = TL.concatMap
    {-# INLINE oconcatMap #-}

-- | A typeclass for monomorphic containers whose elements
-- are an instance of 'Eq'.
class (MonoFoldable mono, Eq (Element mono)) => MonoFoldableEq mono where
    -- | Checks if the monomorphic container includes the supplied element.
    oelem :: Element mono -> mono -> Bool
    oelem e = List.elem e . otoList

    -- | Checks if the monomorphic container does not include the supplied element.
    onotElem :: Element mono -> mono -> Bool
    onotElem e = List.notElem e . otoList
    {-# INLINE oelem #-}
    {-# INLINE onotElem #-}

instance Eq a => MonoFoldableEq (Seq.Seq a)
instance Eq a => MonoFoldableEq (V.Vector a)
instance (Eq a, U.Unbox a) => MonoFoldableEq (U.Vector a)
instance (Eq a, VS.Storable a) => MonoFoldableEq (VS.Vector a)
instance Eq a => MonoFoldableEq (NonEmpty a)
instance MonoFoldableEq T.Text
instance MonoFoldableEq TL.Text
instance MonoFoldableEq IntSet
instance Eq a => MonoFoldableEq (Maybe a)
instance Eq a => MonoFoldableEq (Tree a)
instance Eq a => MonoFoldableEq (ViewL a)
instance Eq a => MonoFoldableEq (ViewR a)
instance Eq a => MonoFoldableEq (IntMap a)
instance Eq a => MonoFoldableEq (Option a)
instance Eq a => MonoFoldableEq (Identity a)
instance Eq v => MonoFoldableEq (Map k v)
instance Eq v => MonoFoldableEq (HashMap k v)
instance Eq a => MonoFoldableEq (HashSet a)
instance Eq a => MonoFoldableEq (DList a)
instance Eq b => MonoFoldableEq (Either a b)
instance Eq b => MonoFoldableEq (a, b)
instance Eq a => MonoFoldableEq (Const m a)
instance (Eq a, F.Foldable f) => MonoFoldableEq (MaybeT f a)
instance (Eq a, F.Foldable f) => MonoFoldableEq (ListT f a)
instance (Eq a, F.Foldable f) => MonoFoldableEq (IdentityT f a)
instance (Eq a, F.Foldable f) => MonoFoldableEq (WriterT w f a)
instance (Eq a, F.Foldable f) => MonoFoldableEq (Strict.WriterT w f a)
instance (Eq a, F.Foldable f) => MonoFoldableEq (ErrorT e f a)
instance (Eq a, F.Foldable f, F.Foldable g) => MonoFoldableEq (Compose f g a)
instance (Eq a, F.Foldable f, F.Foldable g) => MonoFoldableEq (Product f g a)

instance Eq a => MonoFoldableEq [a] where
    oelem = List.elem
    onotElem = List.notElem
    {-# INLINE oelem #-}
    {-# INLINE onotElem #-}

instance MonoFoldableEq S.ByteString where
    oelem = S.elem
    onotElem = S.notElem
    {-# INLINE oelem #-}
    {-# INLINE onotElem #-}

instance MonoFoldableEq L.ByteString where
    oelem = L.elem
    onotElem = L.notElem
    {-# INLINE oelem #-}
    {-# INLINE onotElem #-}

instance (Eq a, Ord a) => MonoFoldableEq (Set a) where
    oelem = Set.member
    onotElem = Set.notMember
    {-# INLINE oelem #-}
    {-# INLINE onotElem #-}


-- | A typeclass for monomorphic containers whose elements
-- are an instance of 'Ord'.
class (MonoFoldable mono, Ord (Element mono)) => MonoFoldableOrd mono where
    -- | Get the minimum element of a monomorphic container.
    --
    -- Note: this is a partial function. On an empty 'MonoFoldable', it will
    -- throw an exception.
    --
    -- /See 'Data.MinLen.maximum' from "Data.MinLen" for a total version of this function./
    maximumEx :: mono -> Element mono
    maximumEx = maximumByEx compare
    {-# INLINE maximumEx #-}

    -- | Get the maximum element of a monomorphic container.
    --
    -- Note: this is a partial function. On an empty 'MonoFoldable', it will
    -- throw an exception.
    --
    -- /See 'Data.MinLen.minimum' from "Data.MinLen" for a total version of this function./
    minimumEx :: mono -> Element mono
    minimumEx = minimumByEx compare
    {-# INLINE minimumEx #-}

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
instance MonoFoldableOrd IntSet
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
    minimumEx   = V.minimum
    {-# INLINE maximumEx #-}
    {-# INLINE minimumEx #-}
instance Ord e => MonoFoldableOrd (Set e)
instance Ord e => MonoFoldableOrd (HashSet e)
instance (U.Unbox a, Ord a) => MonoFoldableOrd (U.Vector a) where
    maximumEx   = U.maximum
    minimumEx   = U.minimum
    {-# INLINE maximumEx #-}
    {-# INLINE minimumEx #-}
instance (Ord a, VS.Storable a) => MonoFoldableOrd (VS.Vector a) where
    maximumEx   = VS.maximum
    minimumEx   = VS.minimum
    {-# INLINE maximumEx #-}
    {-# INLINE minimumEx #-}
instance Ord b => MonoFoldableOrd (Either a b) where
instance Ord a => MonoFoldableOrd (DList a)
instance Ord b => MonoFoldableOrd (a, b)
instance Ord a => MonoFoldableOrd (Const m a)
instance (Ord a, F.Foldable f) => MonoFoldableOrd (MaybeT f a)
instance (Ord a, F.Foldable f) => MonoFoldableOrd (ListT f a)
instance (Ord a, F.Foldable f) => MonoFoldableOrd (IdentityT f a)
instance (Ord a, F.Foldable f) => MonoFoldableOrd (WriterT w f a)
instance (Ord a, F.Foldable f) => MonoFoldableOrd (Strict.WriterT w f a)
instance (Ord a, F.Foldable f) => MonoFoldableOrd (ErrorT e f a)
instance (Ord a, F.Foldable f, F.Foldable g) => MonoFoldableOrd (Compose f g a)
instance (Ord a, F.Foldable f, F.Foldable g) => MonoFoldableOrd (Product f g a)

-- | Safe version of 'maximumEx'.
--
-- Returns 'Nothing' instead of throwing an exception when
-- encountering an empty monomorphic container.
maximumMay :: MonoFoldableOrd mono => mono -> Maybe (Element mono)
maximumMay mono
    | onull mono = Nothing
    | otherwise = Just (maximumEx mono)
{-# INLINE maximumMay #-}

-- | Safe version of 'maximumByEx'.
--
-- Returns 'Nothing' instead of throwing an exception when
-- encountering an empty monomorphic container.
maximumByMay :: MonoFoldable mono
             => (Element mono -> Element mono -> Ordering)
             -> mono
             -> Maybe (Element mono)
maximumByMay f mono
    | onull mono = Nothing
    | otherwise = Just (maximumByEx f mono)
{-# INLINE maximumByMay #-}

-- | Safe version of 'minimumEx'.
--
-- Returns 'Nothing' instead of throwing an exception when
-- encountering an empty monomorphic container.
minimumMay :: MonoFoldableOrd mono => mono -> Maybe (Element mono)
minimumMay mono
    | onull mono = Nothing
    | otherwise = Just (minimumEx mono)
{-# INLINE minimumMay #-}

-- | Safe version of 'minimumByEx'.
--
-- Returns 'Nothing' instead of throwing an exception when
-- encountering an empty monomorphic container.
minimumByMay :: MonoFoldable mono
             => (Element mono -> Element mono -> Ordering)
             -> mono
             -> Maybe (Element mono)
minimumByMay f mono
    | onull mono = Nothing
    | otherwise = Just (minimumByEx f mono)
{-# INLINE minimumByMay #-}

-- | Monomorphic containers that can be traversed from left to right.
class (MonoFunctor mono, MonoFoldable mono) => MonoTraversable mono where
    -- | Map each element of a monomorphic container to an action,
    -- evaluate these actions from left to right, and
    -- collect the results.
    otraverse :: Applicative f => (Element mono -> f (Element mono)) -> mono -> f mono
    default otraverse :: (Traversable t, mono ~ t a, a ~ Element mono, Applicative f) => (Element mono -> f (Element mono)) -> mono -> f mono
    otraverse = traverse

    -- | Map each element of a monomorphic container to a monadic action,
    -- evaluate these actions from left to right, and
    -- collect the results.
    omapM :: Monad m => (Element mono -> m (Element mono)) -> mono -> m mono
    default omapM :: (Traversable t, mono ~ t a, a ~ Element mono, Monad m) => (Element mono -> m (Element mono)) -> mono -> m mono
    omapM = mapM
    {-# INLINE otraverse #-}
    {-# INLINE omapM #-}

instance MonoTraversable S.ByteString where
    otraverse f = fmap S.pack . traverse f . S.unpack
    omapM f = liftM S.pack . mapM f . S.unpack
    {-# INLINE otraverse #-}
    {-# INLINE omapM #-}
instance MonoTraversable L.ByteString where
    otraverse f = fmap L.pack . traverse f . L.unpack
    omapM f = liftM L.pack . mapM f . L.unpack
    {-# INLINE otraverse #-}
    {-# INLINE omapM #-}
instance MonoTraversable T.Text where
    otraverse f = fmap T.pack . traverse f . T.unpack
    omapM f = liftM T.pack . mapM f . T.unpack
    {-# INLINE otraverse #-}
    {-# INLINE omapM #-}
instance MonoTraversable TL.Text where
    otraverse f = fmap TL.pack . traverse f . TL.unpack
    omapM f = liftM TL.pack . mapM f . TL.unpack
    {-# INLINE otraverse #-}
    {-# INLINE omapM #-}
instance MonoTraversable [a]
instance MonoTraversable (Maybe a)
instance MonoTraversable (Tree a)
instance MonoTraversable (Seq a)
instance MonoTraversable (ViewL a)
instance MonoTraversable (ViewR a)
instance MonoTraversable (IntMap a)
instance MonoTraversable (Option a)
instance MonoTraversable (NonEmpty a)
instance MonoTraversable (DList a) where
     otraverse f = fmap DL.fromList . traverse f . DL.toList
     omapM f = liftM DL.fromList . mapM f . DL.toList
instance MonoTraversable (Identity a)
instance MonoTraversable (Map k v)
instance MonoTraversable (HashMap k v)
instance MonoTraversable (Vector a)
instance U.Unbox a => MonoTraversable (U.Vector a) where
    otraverse f = fmap U.fromList . traverse f . U.toList
    omapM = U.mapM
    {-# INLINE otraverse #-}
    {-# INLINE omapM #-}
instance VS.Storable a => MonoTraversable (VS.Vector a) where
    otraverse f = fmap VS.fromList . traverse f . VS.toList
    omapM = VS.mapM
    {-# INLINE otraverse #-}
    {-# INLINE omapM #-}
instance MonoTraversable (Either a b) where
    otraverse _ (Left a) = pure (Left a)
    otraverse f (Right b) = fmap Right (f b)
    omapM _ (Left a) = return (Left a)
    omapM f (Right b) = liftM Right (f b)
    {-# INLINE otraverse #-}
    {-# INLINE omapM #-}
instance MonoTraversable (a, b)
instance MonoTraversable (Const m a)
instance Traversable f => MonoTraversable (MaybeT f a)
instance Traversable f => MonoTraversable (ListT f a)
instance Traversable f => MonoTraversable (IdentityT f a)
instance Traversable f => MonoTraversable (WriterT w f a)
instance Traversable f => MonoTraversable (Strict.WriterT w f a)
instance Traversable f => MonoTraversable (ErrorT e f a)
instance (Traversable f, Traversable g) => MonoTraversable (Compose f g a)
instance (Traversable f, Traversable g) => MonoTraversable (Product f g a)

-- | 'ofor' is 'otraverse' with its arguments flipped.
ofor :: (MonoTraversable mono, Applicative f) => mono -> (Element mono -> f (Element mono)) -> f mono
ofor = flip otraverse
{-# INLINE ofor #-}

-- | 'oforM' is 'omapM' with its arguments flipped.
oforM :: (MonoTraversable mono, Monad f) => mono -> (Element mono -> f (Element mono)) -> f mono
oforM = flip omapM
{-# INLINE oforM #-}

-- | A strict left fold, together with an unwrap function.
--
-- This is convenient when the accumulator value is not the same as the final
-- expected type. It is provided mainly for integration with the @foldl@
-- package, to be used in conjunction with @purely@.
--
-- Since 0.3.1
ofoldlUnwrap :: MonoFoldable mono
             => (x -> Element mono -> x) -> x -> (x -> b) -> mono -> b
ofoldlUnwrap f x unwrap mono = unwrap (ofoldl' f x mono)

-- | A monadic strict left fold, together with an unwrap function.
--
-- Similar to 'foldlUnwrap', but allows monadic actions. To be used with
-- @impurely@ from @foldl@.
--
-- Since 0.3.1
ofoldMUnwrap :: (Monad m, MonoFoldable mono)
             => (x -> Element mono -> m x) -> m x -> (x -> m b) -> mono -> m b
ofoldMUnwrap f mx unwrap mono = do
    x <- mx
    x' <- ofoldlM f x mono
    unwrap x'

-- | Typeclass for monomorphic containers that an element can be
-- lifted into.
--
-- For any 'MonoFunctor', the following law holds:
--
-- @
-- 'omap' f . 'opoint' = 'opoint' . f
-- @
class MonoPointed mono where
    -- | Lift an element into a monomorphic container.
    --
    -- 'opoint' is the same as 'Control.Applicative.pure' for an 'Applicative'
    opoint :: Element mono -> mono
    default opoint :: (Applicative f, (f a) ~ mono, Element (f a) ~ a)
                   => Element mono -> mono
    opoint = pure
    {-# INLINE opoint #-}

-- monomorphic
instance MonoPointed S.ByteString where
    opoint = S.singleton
    {-# INLINE opoint #-}
instance MonoPointed L.ByteString where
    opoint = L.singleton
    {-# INLINE opoint #-}
instance MonoPointed T.Text where
    opoint = T.singleton
    {-# INLINE opoint #-}
instance MonoPointed TL.Text where
    opoint = TL.singleton
    {-# INLINE opoint #-}

-- Applicative
instance MonoPointed [a]
instance MonoPointed (Maybe a)
instance MonoPointed (Option a)
instance MonoPointed (NonEmpty a)
instance MonoPointed (Identity a)
instance MonoPointed (Vector a)
instance MonoPointed (DList a)
instance MonoPointed (IO a)
instance MonoPointed (ZipList a)
instance MonoPointed (r -> a)
instance Monoid a => MonoPointed (a, b)
instance Monoid m => MonoPointed (Const m a)
instance Monad m => MonoPointed (WrappedMonad m a)
instance Applicative m => MonoPointed (ListT m a)
instance Applicative m => MonoPointed (IdentityT m a)
instance Applicative f => MonoPointed (WrappedApplicative f a)
instance Arrow a => MonoPointed (WrappedArrow a b c)
instance (Monoid w, Applicative m) => MonoPointed (WriterT w m a)
instance (Monoid w, Applicative m) => MonoPointed (Strict.WriterT w m a)
instance Applicative m => MonoPointed (ReaderT r m a)
instance MonoPointed (ContT r m a)
instance (Applicative f, Applicative g) => MonoPointed (Compose f g a)
instance (Applicative f, Applicative g) => MonoPointed (Product f g a)
instance MonoPointed (Cokleisli w a b)
instance Applicative f => MonoPointed (Static f a b)

-- Not Applicative
instance MonoPointed (Seq a) where
    opoint = Seq.singleton
    {-# INLINE opoint #-}
instance U.Unbox a => MonoPointed (U.Vector a) where
    opoint = U.singleton
    {-# INLINE opoint #-}
instance VS.Storable a => MonoPointed (VS.Vector a) where
    opoint = VS.singleton
    {-# INLINE opoint #-}
instance MonoPointed (Either a b) where
    opoint = Right
    {-# INLINE opoint #-}
instance MonoPointed IntSet.IntSet where
    opoint = IntSet.singleton
    {-# INLINE opoint #-}
instance MonoPointed (Set a) where
    opoint = Set.singleton
    {-# INLINE opoint #-}
instance Hashable a => MonoPointed (HashSet a) where
    opoint = HashSet.singleton
    {-# INLINE opoint #-}
instance Applicative m => MonoPointed (ErrorT e m a) where
    opoint = ErrorT . pure . Right
    {-# INLINE opoint #-}
instance MonoPointed (MaybeApply f a) where
    opoint = MaybeApply . Right
    {-# INLINE opoint #-}
instance Applicative f => MonoPointed (MaybeT f a) where
    opoint = MaybeT . fmap Just . pure
    {-# INLINE opoint #-}
instance (Monoid w, Applicative m) => MonoPointed (RWST r w s m a) where
    opoint a = RWST (\_ s -> pure (a, s, mempty))
    {-# INLINE opoint #-}
instance (Monoid w, Applicative m) => MonoPointed (Strict.RWST r w s m a) where
    opoint a = Strict.RWST (\_ s -> pure (a, s, mempty))
    {-# INLINE opoint #-}
instance Applicative m => MonoPointed (StateT s m a) where
    opoint a = StateT (\s -> pure (a, s))
    {-# INLINE opoint #-}
instance Applicative m => MonoPointed (Strict.StateT s m a) where
    opoint a = Strict.StateT (\s -> pure (a, s))
    {-# INLINE opoint #-}
instance MonoPointed (ViewL a) where
    opoint a = a :< Seq.empty
    {-# INLINE opoint #-}
instance MonoPointed (ViewR a) where
    opoint a = Seq.empty :> a
    {-# INLINE opoint #-}
instance MonoPointed (Tree a) where
    opoint a = Node a []
    {-# INLINE opoint #-}


-- | Typeclass for monomorphic containers where it is always okay to
-- "extract" a value from with 'oextract', and where you can extrapolate
-- any "extracting" function to be a function on the whole part with
-- 'oextend'.
--
-- 'oextend' and 'oextract' should work together following the laws:
--
-- @
-- 'oextend' 'oextract'      = 'id'
-- 'oextract' . 'oextend' f  = f
-- 'oextend' f . 'oextend' g = 'oextend' (f . 'oextend' g)
-- @
--
-- As an intuition, @'oextend' f@ uses @f@ to "build up" a new @mono@ with
-- pieces from the old one received by @f@.
--
class MonoFunctor mono => MonoComonad mono where
    -- | Extract an element from @mono@.  Can be thought of as a dual
    -- concept to @opoint@.
    oextract :: mono -> Element mono
    -- | "Extend" a @mono -> 'Element' mono@ function to be a @mono ->
    -- mono@; that is, builds a new @mono@ from the old one by using pieces
    -- glimpsed from the given function.
    oextend :: (mono -> Element mono) -> mono -> mono

    default oextract :: (Comonad w, (w a) ~ mono, Element (w a) ~ a)
                   => mono -> Element mono
    oextract = extract
    {-# INLINE oextract #-}
    default oextend :: (Comonad w, (w a) ~ mono, Element (w a) ~ a)
                   => (mono -> Element mono) -> mono -> mono
    oextend = extend
    {-# INLINE oextend #-}

-- Comonad
instance MonoComonad (Tree a)
instance MonoComonad (NonEmpty a)
instance MonoComonad (Identity a)
instance Monoid m => MonoComonad (m -> a)
instance MonoComonad (e, a)
instance MonoComonad (Arg a b)
instance Comonad w => MonoComonad (IdentityT w a)
instance Comonad w => MonoComonad (EnvT e w a)
instance Comonad w => MonoComonad (StoreT s w a)
instance (Comonad w, Monoid m) => MonoComonad (TracedT m w a)
instance (Comonad f, Comonad g) => MonoComonad (Coproduct f g a)

-- Not Comonad
instance MonoComonad (ViewL a) where
    oextract ~(x :< _) = x
    {-# INLINE oextract #-}
    oextend f w@ ~(_ :< xxs) =
        f w :< case Seq.viewl xxs of
                 EmptyL -> Seq.empty
                 xs     -> case oextend f xs of
                             EmptyL  -> Seq.empty
                             y :< ys -> y Seq.<| ys

instance MonoComonad (ViewR a) where
    oextract ~(_ :> x) = x
    {-# INLINE oextract #-}
    oextend f w@ ~(xxs :> _) =
        (case Seq.viewr xxs of
           EmptyR -> Seq.empty
           xs     -> case oextend f xs of
                       EmptyR  -> Seq.empty
                       ys :> y -> ys Seq.|> y
        ) :> f w
