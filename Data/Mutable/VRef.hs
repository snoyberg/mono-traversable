{-# LANGUAGE TypeFamilies #-}
-- | Use 1-length mutable boxed vectors for mutable references.
--
-- Motivated by: <http://stackoverflow.com/questions/27261813/why-is-my-little-stref-int-require-allocating-gigabytes> and ArrayRef.
module Data.Mutable.VRef
    ( -- * Types
      VRef
    , IOVRef
      -- * Functions
    , asVRef
    , MutableRef (..)
    ) where

import           Control.Monad               (liftM)
import           Data.Mutable.Class
import qualified Data.Vector.Generic.Mutable as V
import qualified Data.Vector.Mutable         as VB

-- | A boxed vector reference, supporting any monad.
newtype VRef s a = VRef (VB.MVector s a)

asVRef :: VRef s a -> VRef s a
asVRef x = x
{-# INLINE asVRef #-}

-- | A boxed IO vector reference.
type IOVRef = VRef (PrimState IO)

instance MutableContainer (VRef s a) where
    type MCState (VRef s a) = s
instance MutableRef (VRef s a) where
    type RefElement (VRef s a) = a

    newRef = liftM VRef . V.replicate 1
    {-# INLINE newRef#-}

    readRef (VRef v) = V.unsafeRead v 0
    {-# INLINE readRef #-}

    writeRef (VRef v) = V.unsafeWrite v 0
    {-# INLINE writeRef #-}

    modifyRef (VRef v) f = V.unsafeRead v 0 >>= V.unsafeWrite v 0 . f
    {-# INLINE modifyRef #-}

    modifyRef' = modifyRef
    {-# INLINE modifyRef' #-}
