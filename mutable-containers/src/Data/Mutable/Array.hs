{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Mutable.Array
    ( Array (..)
    , ArrayMemoryProperties (..)
    ) where

import Data.Mutable.Class
import Data.Word
import GHC.TypeLits
import Data.Kind (Constraint)
import Data.Proxy (Proxy(Proxy))
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.Primitive

import Data.Primitive.ByteArray (MutableByteArray, newByteArray, newPinnedByteArray, newAlignedPinnedByteArray, writeByteArray)
import Data.Primitive.Types (Prim)

newtype Array (p :: ArrayMemoryProperties) e s = Array (MutableByteArray s)

data ArrayMemoryProperties = Regular | Pinned | AlignedPinned Nat

instance MutableContainer (Array p e s) where
    type MCState (Array p e s) = s

instance MutableCollection (Array Regular e s) where
    type CollElement (Array Regular e s) = e
    newColl = coerceToArray $ newByteArray 0
instance MutableCollection (Array Pinned e s) where
    type CollElement (Array Pinned e s) = e
    newColl = coerceToArray $ newPinnedByteArray 0
instance KnownNat n => MutableCollection (Array (AlignedPinned n) e s) where
    type CollElement (Array (AlignedPinned n) e s) = e
    newColl = coerceToArray $ newAlignedPinnedByteArray 0 alignment
      where
      alignment = fromIntegral $ natVal $ Proxy @n

type instance CollIndex (Array _ _ _) = Int
instance MutableAllocatedCollection (Array Regular e s) where
    newCollOfSize = coerceToArray . newByteArray
    {-# INLINE newCollOfSize #-}
instance MutableAllocatedCollection (Array Pinned e s) where
    newCollOfSize = coerceToArray . newPinnedByteArray
    {-# INLINE newCollOfSize #-}
instance KnownNat n => MutableAllocatedCollection (Array (AlignedPinned n) e s) where
    newCollOfSize = coerceToArray . flip newAlignedPinnedByteArray alignment
      where
      alignment = fromIntegral $ natVal $ Proxy @n
    {-# INLINE newCollOfSize #-}

coerceToArray :: m (MutableByteArray s) -> m (Array p e s)
coerceToArray = unsafeCoerce

instance (Prim (CollElement (Array p e s)), MutableAllocatedCollection (Array p e s)) => MutableIndexingWrite (Array p e s) where
  writeIndex (Array c) i x = writeByteArray c i x

type IsPow2 :: Nat -> Constraint
type IsPow2 x = IsPow2' (Mod x 2) x
type IsPow2' :: Nat -> Nat -> Constraint
type family IsPow2' m x where
  IsPow2' _ 2 = ()
  IsPow2' 1 x = TypeError (ShowType x :<>: Text " is not a power of 2.")
  IsPow2' 0 x = IsPow2' 0 (Div x 2)
