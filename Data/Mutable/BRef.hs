{-# LANGUAGE TypeFamilies #-}
-- | Use 1-length mutable boxed vectors for mutable references.
--
-- Motivated by: <http://stackoverflow.com/questions/27261813/why-is-my-little-stref-int-require-allocating-gigabytes> and ArrayRef.
module Data.Mutable.BRef
    ( -- * Types
      BRef
    , IOBRef
      -- * Functions
    , asBRef
    , MutableRef (..)
    ) where

import           Control.Monad               (liftM)
import           Data.Monoid                 (Monoid, mempty)
import           Data.MonoTraversable        (Element)
import           Data.Mutable.Class
import           Data.Sequences              (IsSequence)
import qualified Data.Vector.Generic.Mutable as V
import qualified Data.Vector.Mutable         as VB

-- | A boxed vector reference, supporting any monad.
newtype BRef s a = BRef (VB.MVector s a)

asBRef :: BRef s a -> BRef s a
asBRef x = x
{-# INLINE asBRef #-}

-- | A boxed IO vector reference.
type IOBRef = BRef (PrimState IO)

instance MutableContainer (BRef s a) where
    type MCState (BRef s a) = s
instance MutableRef (BRef s a) where
    type RefElement (BRef s a) = a

    newRef = liftM BRef . V.replicate 1
    {-# INLINE newRef#-}

    readRef (BRef v) = V.unsafeRead v 0
    {-# INLINE readRef #-}

    writeRef (BRef v) = V.unsafeWrite v 0
    {-# INLINE writeRef #-}

    modifyRef (BRef v) f = V.unsafeRead v 0 >>= V.unsafeWrite v 0 . f
    {-# INLINE modifyRef #-}

    modifyRef' = modifyRef
    {-# INLINE modifyRef' #-}

instance Monoid w => MutableCollection (BRef s w) where
    type CollElement (BRef s w) = Element w
    newColl = newRef mempty
    {-# INLINE newColl #-}
instance IsSequence seq => MutablePushFront (BRef s seq) where
    pushFront = pushFrontRef
    {-# INLINE pushFront #-}
instance IsSequence seq => MutablePushBack (BRef s seq) where
    pushBack = pushBackRef
    {-# INLINE pushBack #-}
instance IsSequence seq => MutablePopFront (BRef s seq) where
    popFront = popFrontRef
    {-# INLINE popFront #-}
instance IsSequence seq => MutablePopBack (BRef s seq) where
    popBack = popBackRef
    {-# INLINE popBack #-}
