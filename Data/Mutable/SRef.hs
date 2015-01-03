{-# LANGUAGE TypeFamilies #-}
-- | Use 1-length mutable storable vectors for mutable references.
--
-- Motivated by: http://stackoverflow.com/questions/27261813/why-is-my-little-stref-int-require-allocating-gigabytes and ArrayRef.
module Data.Mutable.SRef
    ( -- * Types
      SRef
    , IOSRef
      -- * Functions
    , asSRef
    , MutableRef (..)
    ) where

import Data.Mutable.Class
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (ST)
import Control.Monad (liftM)
import qualified Data.Vector.Storable.Mutable as VS
import qualified Data.Vector.Mutable as VB
import qualified Data.Vector.Generic.Mutable as V

-- | An unboxed vector reference, supporting any monad.
newtype SRef s a = SRef (VS.MVector s a)

asSRef :: SRef s a -> SRef s a
asSRef x = x
{-# INLINE asSRef #-}

-- | An unboxed IO vector reference.
type IOSRef = SRef (PrimState IO)

instance MutableContainer (SRef s a) where
    type MCState (SRef s a) = s
instance VS.Storable a => MutableRef (SRef s a) where
    type RefElement (SRef s a) = a

    newRef = liftM SRef . V.replicate 1
    {-# INLINE newRef#-}

    readRef (SRef v) = V.unsafeRead v 0
    {-# INLINE readRef #-}

    writeRef (SRef v) = V.unsafeWrite v 0
    {-# INLINE writeRef #-}

    modifyRef (SRef v) f = V.unsafeRead v 0 >>= V.unsafeWrite v 0 . f
    {-# INLINE modifyRef #-}

    modifyRef' = modifyRef
    {-# INLINE modifyRef' #-}
