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
    , MutableInitialSizedCollection (..)
    , MutableIndexing (..)
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
import           Control.Monad.ST                  (ST)
import qualified Data.Vector.Mutable               as MV
import qualified Data.Vector.Primitive.Mutable     as MPV
import qualified Data.Vector.Storable.Mutable      as MSV
import qualified Data.Vector.Unboxed.Mutable       as MUV
import qualified GHC.Arr

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
instance MutableContainer (MV.MVector s a) where
    type MCState (MV.MVector s a) = s
instance MutableContainer (MPV.MVector s a) where
    type MCState (MPV.MVector s a) = s
instance MutableContainer (MSV.MVector s a) where
    type MCState (MSV.MVector s a) = s
instance MutableContainer (MUV.MVector s a) where
    type MCState (MUV.MVector s a) = s
instance MutableContainer (GHC.Arr.STArray s i e) where
    type MCState (GHC.Arr.STArray s i e) = s

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
instance MutableCollection (MV.MVector s a) where
    type CollElement (MV.MVector s a) = a
    newColl = MV.new 0
    {-# INLINE newColl #-}
instance MPV.Prim a => MutableCollection (MPV.MVector s a) where
    type CollElement (MPV.MVector s a) = a
    newColl = MPV.new 0
    {-# INLINE newColl #-}
instance MSV.Storable a => MutableCollection (MSV.MVector s a) where
    type CollElement (MSV.MVector s a) = a
    newColl = MSV.new 0
    {-# INLINE newColl #-}
instance MUV.Unbox a => MutableCollection (MUV.MVector s a) where
    type CollElement (MUV.MVector s a) = a
    newColl = MUV.new 0
    {-# INLINE newColl #-}
instance (GHC.Arr.Ix i, Num i) => MutableCollection (GHC.Arr.STArray s i e) where
    type CollElement (GHC.Arr.STArray s i e) = e
    newColl = primToPrim $ GHC.Arr.newSTArray (0,0) undefined
    {-# INLINE newColl #-}

-- | Containers that can be initialized with n elements.
class MutableCollection c => MutableInitialSizedCollection c where
    type CollIndex c
    newCollOfSize :: (PrimMonad m, PrimState m ~ MCState c)
            => CollIndex c
            -> m c
instance MutableInitialSizedCollection (MV.MVector s a) where
    type CollIndex (MV.MVector s a) = Int
    newCollOfSize = MV.new
    {-# INLINE newCollOfSize #-}
instance MPV.Prim a => MutableInitialSizedCollection (MPV.MVector s a) where
    type CollIndex (MPV.MVector s a) = Int
    newCollOfSize = MPV.new
    {-# INLINE newCollOfSize #-}
instance MSV.Storable a => MutableInitialSizedCollection (MSV.MVector s a) where
    type CollIndex (MSV.MVector s a) = Int
    newCollOfSize = MSV.new
    {-# INLINE newCollOfSize #-}
instance MUV.Unbox a => MutableInitialSizedCollection (MUV.MVector s a) where
    type CollIndex (MUV.MVector s a) = Int
    newCollOfSize = MUV.new
    {-# INLINE newCollOfSize #-}
instance (GHC.Arr.Ix i, Num i) => MutableInitialSizedCollection (GHC.Arr.STArray s i e) where
    type CollIndex (GHC.Arr.STArray s i e) = i
    newCollOfSize x = primToPrim $ GHC.Arr.newSTArray (0,x) undefined
    {-# INLINE newCollOfSize #-}

class MutableInitialSizedCollection c => MutableIndexing c where
    readIndex :: (PrimMonad m, PrimState m ~ MCState c) => c -> CollIndex c -> m (CollElement c)
    writeIndex :: (PrimMonad m, PrimState m ~ MCState c) => c -> CollIndex c -> CollElement c -> m ()
instance MutableIndexing (MV.MVector s a) where
    readIndex = MV.read
    {-# INLINE readIndex #-}
    writeIndex = MV.write
    {-# INLINE writeIndex #-}
instance MPV.Prim a => MutableIndexing (MPV.MVector s a) where
    readIndex = MPV.read
    {-# INLINE readIndex #-}
    writeIndex = MPV.write
    {-# INLINE writeIndex #-}
instance MSV.Storable a => MutableIndexing (MSV.MVector s a) where
    readIndex = MSV.read
    {-# INLINE readIndex #-}
    writeIndex = MSV.write
    {-# INLINE writeIndex #-}
instance MUV.Unbox a => MutableIndexing (MUV.MVector s a) where
    readIndex = MUV.read
    {-# INLINE readIndex #-}
    writeIndex = MUV.write
    {-# INLINE writeIndex #-}
instance (GHC.Arr.Ix i, Num i) => MutableIndexing (GHC.Arr.STArray s i e) where
    readIndex c i = primToPrim $ GHC.Arr.readSTArray c i
    {-# INLINE readIndex #-}
    writeIndex c i e = primToPrim $ GHC.Arr.writeSTArray c i e
    {-# INLINE writeIndex #-}

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
