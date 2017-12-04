{-# LANGUAGE TypeFamilies #-}
-- | Doubly-linked list
module Data.Mutable.DLList
    ( DLList
    , asDLList
    , module Data.Mutable.Class
    ) where

import Data.Mutable.Class

data Node s a = Node
    a
    (MutVar s (Maybe (Node s a))) -- previous
    (MutVar s (Maybe (Node s a))) -- next

-- | A doubly-linked list.
--
-- Since 0.3.0
data DLList s a = DLList (MutVar s (Maybe (Node s a))) (MutVar s (Maybe (Node s a)))

-- |
-- Since 0.2.0
asDLList :: DLList s a -> DLList s a
asDLList = id
{-# INLINE asDLList #-}

instance MutableContainer (DLList s a) where
    type MCState (DLList s a) = s
instance MutableCollection (DLList s a) where
    type CollElement (DLList s a) = a
    newColl = do
        x <- newRef $! Nothing
        y <- newRef $! Nothing
        return $! DLList x y
    {-# INLINE newColl #-}
instance MutablePopFront (DLList s a) where
    popFront (DLList frontRef backRef) = do
        mfront <- readRef frontRef
        case mfront of
            Nothing -> return Nothing
            Just (Node val _ nextRef) -> do
                mnext <- readRef nextRef
                case mnext of
                    Nothing -> do
                        writeRef frontRef $! Nothing
                        writeRef backRef $! Nothing
                    Just next@(Node _ prevRef _) -> do
                        writeRef prevRef $! Nothing
                        writeRef frontRef $! Just next
                return $ Just val
    {-# INLINE popFront #-}
instance MutablePopBack (DLList s a) where
    popBack (DLList frontRef backRef) = do
        mback <- readRef backRef
        case mback of
            Nothing -> return Nothing
            Just (Node val prevRef _) -> do
                mprev <- readRef prevRef
                case mprev of
                    Nothing -> do
                        writeRef frontRef $! Nothing
                        writeRef backRef $! Nothing
                    Just prev@(Node _ _ nextRef) -> do
                        writeRef nextRef $! Nothing
                        writeRef backRef (Just prev)
                return $ Just val
    {-# INLINE popBack #-}
instance MutablePushFront (DLList s a) where
    pushFront (DLList frontRef backRef) val = do
        mfront <- readRef frontRef
        case mfront of
            Nothing -> do
                prevRef <- newRef $! Nothing
                nextRef <- newRef $! Nothing
                let node = Just $ Node val prevRef nextRef
                writeRef frontRef node
                writeRef backRef node
            Just front@(Node _ prevRef _) -> do
                prevRefNew <- newRef $! Nothing
                nextRef <- newRef $ Just front
                let node = Just $ Node val prevRefNew nextRef
                writeRef prevRef node
                writeRef frontRef node
    {-# INLINE pushFront #-}
instance MutablePushBack (DLList s a) where
    pushBack (DLList frontRef backRef) val = do
        mback <- readRef backRef
        case mback of
            Nothing -> do
                prevRef <- newRef $! Nothing
                nextRef <- newRef $! Nothing
                let node = Just $! Node val prevRef nextRef
                writeRef frontRef $! node
                writeRef backRef $! node
            Just back@(Node _ _ nextRef) -> do
                nextRefNew <- newRef $! Nothing
                prevRef <- newRef $! Just back
                let node = Just $! Node val prevRef nextRefNew
                writeRef nextRef $! node
                writeRef backRef $! node
    {-# INLINE pushBack #-}
