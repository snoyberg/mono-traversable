{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators #-}
-- | Various typeclasses for mutable containers.
module Data.Mutable.Class
    ( PrimMonad
    , PrimState
    , RealWorld
    , MutableQueue
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

-- | The parent typeclass for all mutable containers.
--
-- Since 0.2.0
class MutableContainer c where
    -- | Associated type giving the primitive state token for the given
    -- container, much like 'PrimState' from primitive.
    --
    -- Since 0.2.0
    type MCState c

instance MutableContainer (IORef a) where
    type MCState (IORef a) = PrimState IO
instance MutableContainer (STRef s a) where
    type MCState (STRef s a) = s
instance MutableContainer (MutVar s a) where
    type MCState (MutVar s a) = s

-- | Typeclass for single-cell mutable references.
--
-- Since 0.2.0
class MutableContainer c => MutableRef c where
    -- | Associated type giving the type of the value inside the mutable
    -- reference.
    --
    -- Since 0.2.0
    type RefElement c

    -- | Create a new mutable reference with the given value.
    --
    -- Since 0.2.0
    newRef :: (PrimMonad m, PrimState m ~ MCState c)
           => RefElement c
           -> m c

    -- | Read the current value in the mutable reference.
    --
    -- Since 0.2.0
    readRef :: (PrimMonad m, PrimState m ~ MCState c)
            => c
            -> m (RefElement c)

    -- | Write a new value to the mutable reference.
    --
    -- Since 0.2.0
    writeRef :: (PrimMonad m, PrimState m ~ MCState c)
             => c
             -> RefElement c
             -> m ()

    -- | Modify the value in the mutable reference, without necessarily forcing the result.
    --
    -- Note: some implementations /will/ force the result, in particular
    -- @PRef@, @SRef@, and @URef@.
    --
    -- Since 0.2.0
    modifyRef :: (PrimMonad m, PrimState m ~ MCState c)
              => c
              -> (RefElement c -> RefElement c)
              -> m ()

    -- | Modify the value in the mutable reference, forcing the result.
    --
    -- Since 0.2.0
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

-- | @MutableRef@s that provide for atomic modifications of their contents.
--
-- Since 0.2.0
class MutableRef c => MutableAtomicRef c where
    -- | Modify the value without necessarily forcing the result.
    --
    -- Since 0.2.0
    atomicModifyRef
        :: (PrimMonad m, PrimState m ~ MCState c)
        => c
        -> (RefElement c -> (RefElement c, a))
        -> m a

    -- | Modify the value, forcing the result.
    --
    -- Since 0.2.0
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

-- | Containers which contain 0 or more values.
--
-- Since 0.2.0
class MutableContainer c => MutableCollection c where
    -- | The type of each value in the collection.
    --
    -- Since 0.2.0
    type CollElement c

    -- | Create a new, empty collection.
    --
    -- Since 0.2.0
    newColl :: (PrimMonad m, PrimState m ~ MCState c)
            => m c
instance Data.Monoid.Monoid w => MutableCollection (IORef w) where
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

-- | Take a value from the front of the collection, if available.
--
-- Since 0.2.0
class MutableCollection c => MutablePopFront c where
    -- | Take a value from the front of the collection, if available.
    --
    -- Since 0.2.0
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

-- | Place a value at the front of the collection.
--
-- Since 0.2.0
class MutableCollection c => MutablePushFront c where
    -- | Place a value at the front of the collection.
    --
    -- Since 0.2.0
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

-- | Take a value from the back of the collection, if available.
--
-- Since 0.2.0
class MutableCollection c => MutablePopBack c where
    -- | Take a value from the back of the collection, if available.
    --
    -- Since 0.2.0
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

-- | Place a value at the back of the collection.
--
-- Since 0.2.0
class MutableCollection c => MutablePushBack c where
    -- | Place a value at the back of the collection.
    --
    -- Since 0.2.0
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

-- | Collections which allow pushing and popping at the front (aka FIFOs).
--
-- Since 0.2.0
type MutableQueue c = (MutablePopFront c, MutablePushBack c)

-- | Collections which allow pushing at the back and popping at the front (aka FILOs).
--
-- Since 0.2.0
type MutableStack c = (MutablePopFront c, MutablePushFront c)

-- | Collections which allow pushing and popping at the front and back.
--
-- Since 0.2.0
type MutableDeque c = (MutableQueue c, MutablePushFront c, MutablePopBack c)

-- |
-- Since 0.2.0
asIORef :: IORef a -> IORef a
asIORef = id

-- |
-- Since 0.2.0
asSTRef :: STRef s a -> STRef s a
asSTRef = id

-- |
-- Since 0.2.0
asMutVar :: MutVar s a -> MutVar s a
asMutVar = id
