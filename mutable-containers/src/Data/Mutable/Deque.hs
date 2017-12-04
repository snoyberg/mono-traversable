{-# LANGUAGE TypeFamilies #-}
module Data.Mutable.Deque
    ( Deque
    , UDeque
    , asUDeque
    , SDeque
    , asSDeque
    , BDeque
    , asBDeque
    , module Data.Mutable.Class
    ) where

import           Control.Exception            (assert)
import           Control.Monad                (liftM)
import           Data.Mutable.Class
import qualified Data.Vector.Generic.Mutable  as V
import qualified Data.Vector.Mutable          as B
import qualified Data.Vector.Storable.Mutable as S
import qualified Data.Vector.Unboxed.Mutable  as U

data DequeState v s a = DequeState
    (v s a)
    {-# UNPACK #-} !Int -- start
    {-# UNPACK #-} !Int -- size

-- | A double-ended queue supporting any underlying vector type and any monad.
--
-- This implements a circular double-ended queue with exponential growth.
--
-- Since 0.2.0
newtype Deque v s a = Deque (MutVar s (DequeState v s a))

-- | A 'Deque' specialized to unboxed vectors.
--
-- Since 0.2.0
type UDeque = Deque U.MVector

-- | A 'Deque' specialized to storable vectors.
--
-- Since 0.2.0
type SDeque = Deque S.MVector

-- | A 'Deque' specialized to boxed vectors.
--
-- Since 0.2.0
type BDeque = Deque B.MVector

-- |
-- Since 0.2.0
asUDeque :: UDeque s a -> UDeque s a
asUDeque = id

-- |
-- Since 0.2.0
asSDeque :: SDeque s a -> SDeque s a
asSDeque = id

-- |
-- Since 0.2.0
asBDeque :: BDeque s a -> BDeque s a
asBDeque = id

instance MutableContainer (Deque v s a) where
    type MCState (Deque v s a) = s
instance V.MVector v a => MutableCollection (Deque v s a) where
    type CollElement (Deque v s a) = a
    newColl = do
        v <- V.new baseSize
        liftM Deque $ newRef (DequeState v 0 0)
      where
        baseSize = 32
    {-# INLINE newColl #-}
instance V.MVector v a => MutablePopFront (Deque v s a) where
    popFront (Deque var) = do
        DequeState v start size <- readRef var
        if size == 0
            then return Nothing
            else do
                x <- V.unsafeRead v start
                let start' = start + 1
                    start''
                        | start' >= V.length v = 0
                        | otherwise = start'
                writeRef var $! DequeState v start'' (size - 1)
                return $! Just x
    {-# INLINE popFront #-}
instance V.MVector v a => MutablePopBack (Deque v s a) where
    popBack (Deque var) = do
        DequeState v start size <- readRef var
        if size == 0
            then return Nothing
            else do
                let size' = size - 1
                    end = start + size'
                    end'
                        | end >= V.length v = end - V.length v
                        | otherwise = end
                x <- V.unsafeRead v end'
                writeRef var $! DequeState v start size'
                return $! Just x
    {-# INLINE popBack #-}
instance V.MVector v a => MutablePushFront (Deque v s a) where
    pushFront (Deque var) x = do
        DequeState v start size <- readRef var
        inner v start size
      where
        inner v start size = do
            if size >= V.length v
                then newVector v start size inner
                else do
                    let size' = size + 1
                        start' = (start - 1) `rem` V.length v
                        start''
                            | start' < 0 = V.length v + start'
                            | otherwise = start'
                    V.unsafeWrite v start'' x
                    writeRef var $! DequeState v start'' size'
    {-# INLINE pushFront #-}
instance V.MVector v a => MutablePushBack (Deque v s a) where
    pushBack (Deque var) x = do
        DequeState v start size <- readRef var
        inner v start size
      where
        inner v start size = do
            if size >= V.length v
                then newVector v start size inner
                else do
                    let end = start + size
                        end'
                            | end >= V.length v = end - V.length v
                            | otherwise = end
                    V.unsafeWrite v end' x
                    writeRef var $! DequeState v start (size + 1)
    {-# INLINE pushBack #-}

newVector :: (PrimMonad m, V.MVector v a)
          => v (PrimState m) a
          -> Int
          -> Int
          -> (v (PrimState m) a -> Int -> Int -> m b)
          -> m b
newVector v size2 sizeOrig f = assert (sizeOrig == V.length v) $ do
    v' <- V.unsafeNew (V.length v * 2)
    let size1 = V.length v - size2
    V.unsafeCopy
        (V.unsafeTake size1 v')
        (V.unsafeSlice size2 size1 v)
    V.unsafeCopy
        (V.unsafeSlice size1 size2 v')
        (V.unsafeTake size2 v)
    f v' 0 sizeOrig
{-# INLINE newVector #-}
