{-# LANGUAGE MagicHash    #-}
{-# LANGUAGE TypeFamilies #-}
-- | Use @ByteArray@s containing one element for mutable references.
--
-- This is similar to @URef@s, but avoids the overhead of storing the length of
-- the @Vector@, which we statically know will always be 1. This allows it to
-- be a bit faster.
--
-- Motivated by: <http://stackoverflow.com/questions/27261813/why-is-my-little-stref-int-require-allocating-gigabytes> and ArrayRef.
module Data.Mutable.PRef
    ( -- * Types
      PRef
    , IOPRef
      -- * Functions
    , asPRef
    , MutableRef (..)
    ) where

import Data.Mutable.Class
import Data.Primitive           (sizeOf)
import Data.Primitive.ByteArray (MutableByteArray, newByteArray, readByteArray,
                                 writeByteArray)
import Data.Primitive.Types     (Prim)

-- | A primitive ByteArray reference, supporting any monad.
--
-- Since 0.2.0
newtype PRef s a = PRef (MutableByteArray s)

-- |
-- Since 0.2.0
asPRef :: PRef s a -> PRef s a
asPRef x = x
{-# INLINE asPRef #-}

-- | A primitive ByteArray IO reference.
type IOPRef = PRef (PrimState IO)

instance MutableContainer (PRef s a) where
    type MCState (PRef s a) = s
instance Prim a => MutableRef (PRef s a) where
    type RefElement (PRef s a) = a

    newRef x = do
        ba <- newByteArray (sizeOf $! x)
        writeByteArray ba 0 x
        return $! PRef ba
    {-# INLINE newRef #-}

    readRef (PRef ba) = readByteArray ba 0
    {-# INLINE readRef #-}

    writeRef (PRef ba) = writeByteArray ba 0
    {-# INLINE writeRef #-}

    modifyRef (PRef ba) f = do
        x <- readByteArray ba 0
        writeByteArray ba 0 $! f x
    {-# INLINE modifyRef #-}

    modifyRef' = modifyRef
    {-# INLINE modifyRef' #-}
