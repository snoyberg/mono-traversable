{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
-- | Various typeclasses for mutable containers.
module Data.Mutable.Class
    ( PrimMonad
    , PrimState
    , RealWorld
    , MutableStack
    , MutableDeque
    , IORef
    , asIORef
    , STRef
    , asSTRef
    , MutVar
    , asMutVar
    , MutableContainer (..)
    , MutableRef (..)
    , MutableAtomicRef (..)
    , MutableCollection (..)
    , MutablePushFront (..)
    , MutablePushBack (..)
    , MutablePopFront (..)
    , MutablePopBack (..)
    , pushFrontRef
    , pushBackRef
    , popFrontRef
    , popBackRef
    ) where

import           Control.Monad.Primitive
import           Data.IORef
import           Data.Monoid
import           Data.MonoTraversable    (Element)
import           Data.Primitive.MutVar
import qualified Data.Sequences          as Seqs
import           Data.STRef

class MutableContainer c where
    type MCState c

instance MutableContainer (IORef a) where
    type MCState (IORef a) = PrimState IO
instance MutableContainer (STRef s a) where
    type MCState (STRef s a) = s
instance MutableContainer (MutVar s a) where
    type MCState (MutVar s a) = s

class MutableContainer c => MutableRef c where
    type RefElement c
    newRef :: (PrimMonad m, PrimState m ~ MCState c)
           => RefElement c
           -> m c
    readRef :: (PrimMonad m, PrimState m ~ MCState c)
            => c
            -> m (RefElement c)
    writeRef :: (PrimMonad m, PrimState m ~ MCState c)
             => c
             -> RefElement c
             -> m ()
    modifyRef :: (PrimMonad m, PrimState m ~ MCState c)
              => c
              -> (RefElement c -> RefElement c)
              -> m ()
    modifyRef' :: (PrimMonad m, PrimState m ~ MCState c)
               => c
               -> (RefElement c -> RefElement c)
               -> m ()

instance MutableRef (IORef a) where
    type RefElement (IORef a) = a
    newRef = primToPrim . newIORef
    {-# INLINE newRef #-}
    readRef = primToPrim . readIORef
    {-# INLINE readRef #-}
    writeRef c = primToPrim . writeIORef c
    {-# INLINE writeRef #-}
    modifyRef c = primToPrim . modifyIORef c
    {-# INLINE modifyRef #-}
    modifyRef' c = primToPrim . modifyIORef' c
    {-# INLINE modifyRef' #-}
instance MutableRef (STRef s a) where
    type RefElement (STRef s a) = a
    newRef = primToPrim . newSTRef
    {-# INLINE newRef #-}
    readRef = primToPrim . readSTRef
    {-# INLINE readRef #-}
    writeRef c = primToPrim . writeSTRef c
    {-# INLINE writeRef #-}
    modifyRef c = primToPrim . modifySTRef c
    {-# INLINE modifyRef #-}
    modifyRef' c = primToPrim . modifySTRef' c
    {-# INLINE modifyRef' #-}
instance MutableRef (MutVar s a) where
    type RefElement (MutVar s a) = a
    newRef = newMutVar
    {-# INLINE newRef #-}
    readRef = readMutVar
    {-# INLINE readRef #-}
    writeRef = writeMutVar
    {-# INLINE writeRef #-}
    modifyRef = modifyMutVar
    {-# INLINE modifyRef #-}
    modifyRef' = modifyMutVar'
    {-# INLINE modifyRef' #-}

class MutableRef c => MutableAtomicRef c where
    atomicModifyRef
        :: (PrimMonad m, PrimState m ~ MCState c)
        => c
        -> (RefElement c -> (RefElement c, a))
        -> m a
    atomicModifyRef'
        :: (PrimMonad m, PrimState m ~ MCState c)
        => c
        -> (RefElement c -> (RefElement c, a))
        -> m a
instance MutableAtomicRef (IORef a) where
    atomicModifyRef c = primToPrim . atomicModifyIORef c
    {-# INLINE atomicModifyRef #-}
    atomicModifyRef' c = primToPrim . atomicModifyIORef' c
    {-# INLINE atomicModifyRef' #-}
instance MutableAtomicRef (MutVar s a) where
    atomicModifyRef = atomicModifyMutVar
    {-# INLINE atomicModifyRef #-}
    atomicModifyRef' = atomicModifyMutVar'
    {-# INLINE atomicModifyRef' #-}

class MutableContainer c => MutableCollection c where
    type CollElement c
    newColl :: (PrimMonad m, PrimState m ~ MCState c)
            => m c
instance Monoid w => MutableCollection (IORef w) where
    type CollElement (IORef w) = Element w
    newColl = newRef mempty
    {-# INLINE newColl #-}
instance Monoid w => MutableCollection (STRef s w) where
    type CollElement (STRef s w) = Element w
    newColl = newRef mempty
    {-# INLINE newColl #-}
instance Monoid w => MutableCollection (MutVar s w) where
    type CollElement (MutVar s w) = Element w
    newColl = newRef mempty
    {-# INLINE newColl #-}

class MutableCollection c => MutablePopFront c where
    popFront :: (PrimMonad m, PrimState m ~ MCState c)
             => c
             -> m (Maybe (CollElement c))
popFrontRef
    :: ( PrimMonad m
       , PrimState m ~ MCState c
       , MutableRef c
       , CollElement c ~ Element (RefElement c)
       , Seqs.IsSequence (RefElement c)
       )
    => c
    -> m (Maybe (CollElement c))
popFrontRef c = do
    l <- readRef c
    case Seqs.uncons l of
        Nothing -> return Nothing
        Just (x, xs) -> do
            writeRef c xs
            return (Just x)
{-# INLINE popFrontRef #-}
instance Seqs.IsSequence a => MutablePopFront (IORef a) where
    popFront = popFrontRef
    {-# INLINE popFront #-}
instance Seqs.IsSequence a => MutablePopFront (STRef s a) where
    popFront = popFrontRef
    {-# INLINE popFront #-}
instance Seqs.IsSequence a => MutablePopFront (MutVar s a) where
    popFront = popFrontRef
    {-# INLINE popFront #-}

class MutableCollection c => MutablePushFront c where
    pushFront :: (PrimMonad m, PrimState m ~ MCState c)
              => c
              -> CollElement c
              -> m ()
pushFrontRef
    :: ( PrimMonad m
       , PrimState m ~ MCState c
       , MutableRef c
       , CollElement c ~ Element (RefElement c)
       , Seqs.IsSequence (RefElement c)
       )
    => c
    -> CollElement c
    -> m ()
pushFrontRef c e = modifyRef' c (Seqs.cons e)
{-# INLINE pushFrontRef #-}
instance Seqs.IsSequence a => MutablePushFront (IORef a) where
    pushFront = pushFrontRef
    {-# INLINE pushFront #-}
instance Seqs.IsSequence a => MutablePushFront (STRef s a) where
    pushFront = pushFrontRef
    {-# INLINE pushFront #-}
instance Seqs.IsSequence a => MutablePushFront (MutVar s a) where
    pushFront = pushFrontRef
    {-# INLINE pushFront #-}

class MutableCollection c => MutablePopBack c where
    popBack :: (PrimMonad m, PrimState m ~ MCState c)
            => c
            -> m (Maybe (CollElement c))
popBackRef
    :: ( PrimMonad m
       , PrimState m ~ MCState c
       , MutableRef c
       , CollElement c ~ Element (RefElement c)
       , Seqs.IsSequence (RefElement c)
       )
    => c
    -> m (Maybe (CollElement c))
popBackRef c = do
    l <- readRef c
    case Seqs.unsnoc l of
        Nothing -> return Nothing
        Just (xs, x) -> do
            writeRef c xs
            return (Just x)
{-# INLINE popBackRef #-}
instance Seqs.IsSequence a => MutablePopBack (IORef a) where
    popBack = popBackRef
    {-# INLINE popBack #-}
instance Seqs.IsSequence a => MutablePopBack (STRef s a) where
    popBack = popBackRef
    {-# INLINE popBack #-}
instance Seqs.IsSequence a => MutablePopBack (MutVar s a) where
    popBack = popBackRef
    {-# INLINE popBack #-}

class MutableCollection c => MutablePushBack c where
    pushBack :: (PrimMonad m, PrimState m ~ MCState c)
             => c
             -> CollElement c
             -> m ()
pushBackRef
    :: ( PrimMonad m
       , PrimState m ~ MCState c
       , MutableRef c
       , CollElement c ~ Element (RefElement c)
       , Seqs.IsSequence (RefElement c)
       )
    => c
    -> CollElement c
    -> m ()
pushBackRef c e = modifyRef' c (`Seqs.snoc` e)
{-# INLINE pushBackRef #-}
instance Seqs.IsSequence a => MutablePushBack (IORef a) where
    pushBack = pushBackRef
    {-# INLINE pushBack #-}
instance Seqs.IsSequence a => MutablePushBack (STRef s a) where
    pushBack = pushBackRef
    {-# INLINE pushBack #-}
instance Seqs.IsSequence a => MutablePushBack (MutVar s a) where
    pushBack = pushBackRef
    {-# INLINE pushBack #-}

type MutableQueue c = (MutablePopFront c, MutablePushBack c)
type MutableStack c = (MutablePopFront c, MutablePushFront c)
type MutableDeque c = (MutableQueue c, MutablePushFront c, MutablePopBack c)

asIORef :: IORef a -> IORef a
asIORef = id

asSTRef :: STRef s a -> STRef s a
asSTRef = id

asMutVar :: MutVar s a -> MutVar s a
asMutVar = id
