{-# LANGUAGE TypeFamilies #-}
-- | Doubly-linked list
module Data.Mutable.DList
    ( DList
    , asDList
    , module Data.Mutable.Class
    ) where

import Data.Mutable.Class
import Data.Primitive.MutVar

data Node s a = Node
    a
    (MutVar s (Maybe (Node s a))) -- previous
    (MutVar s (Maybe (Node s a))) -- next

data DList s a = DList (MutVar s (Maybe (Node s a))) (MutVar s (Maybe (Node s a)))

asDList :: DList s a -> DList s a
asDList = id
{-# INLINE asDList #-}

instance MutableContainer (DList s a) where
    type MCState (DList s a) = s
instance MutableCollection (DList s a) where
    type CollElement (DList s a) = a
    newColl = do
        x <- newRef $! Nothing
        y <- newRef $! Nothing
        return $! DList x y
instance MutablePopFront (DList s a) where
    popFront (DList frontRef backRef) = do
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
instance MutablePopBack (DList s a) where
    popBack (DList frontRef backRef) = do
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
instance MutablePushFront (DList s a) where
    pushFront (DList frontRef backRef) val = do
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
instance MutablePushBack (DList s a) where
    pushBack (DList frontRef backRef) val = do
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
