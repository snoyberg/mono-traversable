{-# LANGUAGE TypeFamilies #-}
-- | Use 1-length mutable unboxed vectors for mutable references.
--
-- Motivated by: <http://stackoverflow.com/questions/27261813/why-is-my-little-stref-int-require-allocating-gigabytes> and ArrayRef.
module Data.Mutable.URef
    ( -- * Types
      URef
    , IOURef
      -- * Functions
    , asURef
    , MutableRef (..)
    ) where

import           Control.Monad               (liftM)
import           Data.Mutable.Class
import qualified Data.Vector.Generic.Mutable as V
import qualified Data.Vector.Unboxed.Mutable as VU

-- | An unboxed vector reference, supporting any monad.
newtype URef s a = URef (VU.MVector s a)

asURef :: URef s a -> URef s a
asURef x = x
{-# INLINE asURef #-}

-- | An unboxed IO vector reference.
type IOURef = URef (PrimState IO)

instance MutableContainer (URef s a) where
    type MCState (URef s a) = s
instance VU.Unbox a => MutableRef (URef s a) where
    type RefElement (URef s a) = a

    newRef = liftM URef . V.replicate 1
    {-# INLINE newRef#-}

    readRef (URef v) = V.unsafeRead v 0
    {-# INLINE readRef #-}

    writeRef (URef v) = V.unsafeWrite v 0
    {-# INLINE writeRef #-}

    modifyRef (URef v) f = V.unsafeRead v 0 >>= V.unsafeWrite v 0 . f
    {-# INLINE modifyRef #-}

    modifyRef' = modifyRef
    {-# INLINE modifyRef' #-}
