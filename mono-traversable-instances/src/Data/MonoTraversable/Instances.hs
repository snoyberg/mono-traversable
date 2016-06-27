{-# LANGUAGE CPP                     #-}
{-# LANGUAGE TypeFamilies            #-}
{-# OPTIONS_GHC -fno-warn-orphans    #-}
module Data.MonoTraversable.Instances () where

import Data.DList.Instances ()
import           Data.Traversable.Instances ()
import Data.Functor.Apply (MaybeApply (..), WrappedApplicative)
import Control.Comonad (Cokleisli, Comonad, extract, extend)
import Control.Comonad.Store (StoreT)
import Control.Comonad.Env (EnvT)
import Control.Comonad.Traced (TracedT)
import Data.DList (DList)
import Data.Semigroupoid.Static (Static)
import qualified Data.DList as DL
import Data.Vector.Instances ()
import Data.MonoTraversable
import Data.Sequences
#if !MIN_VERSION_base(4,8,0)
import Control.Monad (liftM)
#endif
import Control.Monad.Trans.Identity (IdentityT)
import Data.Semigroup (Arg)
import Data.List.NonEmpty (NonEmpty)
import Data.Functor.Identity (Identity)
import Data.Tree (Tree)
import Data.Traversable (traverse)
import Control.Applicative (Applicative)
import Data.Monoid (Monoid)

#if !MIN_VERSION_comonad(5,0,0)
import Data.Functor.Coproduct (Coproduct)
#endif

type instance Element (DList a) = a
instance MonoFoldable (DList a) where
    otoList = DL.toList
    headEx = DL.head
    {-# INLINE otoList #-}
    {-# INLINE headEx #-}
instance MonoTraversable (DList a) where
     otraverse f = fmap DL.fromList . Data.Traversable.traverse f . DL.toList
#if !MIN_VERSION_base(4,8,0)
     omapM f = liftM DL.fromList . mapM f . DL.toList
#endif
instance MonoFunctor (DList a)
instance Eq a => MonoFoldableEq (DList a)
instance Ord a => MonoFoldableOrd (DList a)
instance MonoPointed (DList a)
instance GrowingAppend (DList a)

instance SemiSequence (DList a) where
    type Index (DList a) = Int
    cons = DL.cons
    snoc = DL.snoc

    reverse = defaultReverse
    sortBy = defaultSortBy
    intersperse = defaultIntersperse
    find = defaultFind
    {-# INLINE intersperse #-}
    {-# INLINE reverse #-}
    {-# INLINE find #-}
    {-# INLINE sortBy #-}
    {-# INLINE cons #-}
    {-# INLINE snoc #-}

instance IsSequence (DList a) where
    fromList = DL.fromList
    replicate = DL.replicate
    tailEx = DL.tail
    {-# INLINE fromList #-}
    {-# INLINE replicate #-}
    {-# INLINE tailEx #-}

type instance Element (Cokleisli w a b) = b
instance MonoFunctor (Cokleisli w a b)
instance MonoPointed (Cokleisli w a b)

type instance Element (WrappedApplicative f a) = a
instance Control.Applicative.Applicative f => MonoPointed (WrappedApplicative f a)
instance Functor f => MonoFunctor (WrappedApplicative f a)

type instance Element (MaybeApply f a) = a
instance Functor f => MonoFunctor (MaybeApply f a)
instance MonoPointed (MaybeApply f a) where
    opoint = MaybeApply . Right
    {-# INLINE opoint #-}

type instance Element (TracedT m w a) = a
instance (Comonad w, Data.Monoid.Monoid m) => MonoComonad (TracedT m w a) where
    oextract = extract
    oextend = extend
instance Functor w => MonoFunctor (TracedT m w a)

type instance Element (StoreT s w a) = a
instance Comonad w => MonoComonad (StoreT s w a) where
    oextract = extract
    oextend = extend
instance Functor w => MonoFunctor (StoreT s w a)

type instance Element (EnvT e w a) = a
instance Comonad w => MonoComonad (EnvT e w a) where
    oextract = extract
    oextend = extend
instance Functor w => MonoFunctor (EnvT e w a)

#if !MIN_VERSION_comonad(5,0,0)
type instance Element (Coproduct f g a) = a
instance (Functor f, Functor g) => MonoFunctor (Coproduct f g a)
instance (Comonad f, Comonad g) => MonoComonad (Coproduct f g a) where
    oextract = extract
    oextend = extend
#endif

type instance Element (Static f a b) = b
instance Applicative f => MonoPointed (Static f a b)
instance Functor f => MonoFunctor (Static f a b)

instance Comonad w => MonoComonad (IdentityT w a) where
    oextract = extract
    oextend = extend

-- Comonad
instance MonoComonad (Tree a) where
    oextract = extract
    oextend = extend
instance MonoComonad (NonEmpty a) where
    oextract = extract
    oextend = extend
instance MonoComonad (Identity a) where
    oextract = extract
    oextend = extend
instance Monoid m => MonoComonad (m -> a) where
    oextract = extract
    oextend = extend
instance MonoComonad (e, a) where
    oextract = extract
    oextend = extend
instance MonoComonad (Arg a b) where
    oextract = extract
    oextend = extend
