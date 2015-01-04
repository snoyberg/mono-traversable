{-# LANGUAGE TypeFamilies #-}
module Data.Mutable.Deque
    ( Deque
    , asUDeque
    , asSDeque
    , asBDeque
    , module Data.Mutable.Class
    ) where

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

newtype Deque v s a = Deque (MutVar s (DequeState v s a))

asUDeque :: Deque U.MVector s a -> Deque U.MVector s a
asUDeque = id

asSDeque :: Deque S.MVector s a -> Deque S.MVector s a
asSDeque = id

asBDeque :: Deque B.MVector s a -> Deque B.MVector s a
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
                x <- V.unsafeRead v (start `mod` V.length v)
                writeRef var $! DequeState v (start + 1) (size - 1)
                return $! Just x
    {-# INLINE popFront #-}
instance V.MVector v a => MutablePopBack (Deque v s a) where
    popBack (Deque var) = do
        DequeState v start size <- readRef var
        if size == 0
            then return Nothing
            else do
                let size' = size - 1
                    end = (start + size') `mod` V.length v
                x <- V.unsafeRead v end
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
                        start' = (start - 1) `mod` V.length v
                    V.unsafeWrite v start' x
                    writeRef var $! DequeState v start' size'
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
                    let end = (start + size) `mod` V.length v
                    V.unsafeWrite v end x
                    writeRef var $! DequeState v start (size + 1)
    {-# INLINE pushBack #-}

newVector :: (PrimMonad m, V.MVector v a)
          => v (PrimState m) a
          -> Int
          -> Int
          -> (v (PrimState m) a -> Int -> Int -> m b)
          -> m b
newVector v start size f = do
    v' <- V.unsafeNew (V.length v * 2)
    let size1 = V.length v - size2
        size2 = start `mod` V.length v
    V.unsafeCopy
        (V.unsafeTake size1 v')
        (V.unsafeDrop start v)
    V.unsafeCopy
        (V.unsafeSlice size1 size2 v')
        (V.unsafeTake size2 v)
    f v' 0 size
{-# INLINE newVector #-}
