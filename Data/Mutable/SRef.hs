{-# LANGUAGE TypeFamilies #-}
-- | Use 1-length mutable storable vectors for mutable references.
--
-- Motivated by: <http://stackoverflow.com/questions/27261813/why-is-my-little-stref-int-require-allocating-gigabytes> and ArrayRef.
module Data.Mutable.SRef
    ( -- * Types
      SRef
    , IOSRef
      -- * Functions
    , asSRef
    , MutableRef (..)
    ) where

import           Data.Mutable.Class
import Foreign.ForeignPtr
import Foreign.Storable
import Control.Monad.Primitive

-- | A storable vector reference, supporting any monad.
--
-- Since 0.2.0
newtype SRef s a = SRef (ForeignPtr a)

-- |
-- Since 0.2.0
asSRef :: SRef s a -> SRef s a
asSRef x = x
{-# INLINE asSRef #-}

-- | A storable IO vector reference.
type IOSRef = SRef (PrimState IO)

instance MutableContainer (SRef s a) where
    type MCState (SRef s a) = s
instance Storable a => MutableRef (SRef s a) where
    type RefElement (SRef s a) = a

    newRef x = unsafePrimToPrim $ do
        fptr <- mallocForeignPtr
        withForeignPtr fptr $ flip poke x
        return $! SRef fptr
    {-# INLINE newRef#-}

    readRef (SRef fptr) = unsafePrimToPrim $ withForeignPtr fptr peek
    {-# INLINE readRef #-}

    writeRef (SRef fptr) x = unsafePrimToPrim $ withForeignPtr fptr $ flip poke x
    {-# INLINE writeRef #-}

    modifyRef (SRef fptr) f = unsafePrimToPrim $ withForeignPtr fptr $ \ptr ->
        peek ptr >>= poke ptr . f
    {-# INLINE modifyRef #-}

    modifyRef' = modifyRef
    {-# INLINE modifyRef' #-}
