-- | Provides conversion functions between strict 'ByteString's and storable
-- 'Vector's.
module Data.ByteVector
    ( toByteVector
    , fromByteVector
    ) where

import           Data.ByteString.Internal (ByteString (PS))
import           Data.Vector.Storable     (Vector, unsafeFromForeignPtr,
                                           unsafeToForeignPtr)
import           Data.Word                (Word8)

-- | Convert a 'ByteString' into a storable 'Vector'.
--
-- Since 0.6.1
toByteVector :: ByteString -> Vector Word8
toByteVector (PS fptr offset idx) = unsafeFromForeignPtr fptr offset idx
{-# INLINE toByteVector #-}

-- | Convert a storable 'Vector' into a 'ByteString'.
--
-- Since 0.6.1
fromByteVector :: Vector Word8 -> ByteString
fromByteVector v =
    PS fptr offset idx
  where
    (fptr, offset, idx) = unsafeToForeignPtr v
{-# INLINE fromByteVector #-}
