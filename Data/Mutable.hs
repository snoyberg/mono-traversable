-- | Classes and concrete implementations for mutable data structures.
--
-- For more information on the design of this library, see the README file,
-- also available at <http://www.stackage.org/package/mutable-containers>.
module Data.Mutable
    ( -- * Data types
      -- ** Single-cell mutable references
      PRef
    , asPRef
    , URef
    , asURef
    , SRef
    , asSRef
    , BRef
    , asBRef
      -- *** Standard re-exports
    , IORef
    , asIORef
    , STRef
    , asSTRef
    , MutVar
    , asMutVar
      -- ** Collections/queues
    , Deque
    , UDeque
    , asUDeque
    , SDeque
    , asSDeque
    , BDeque
    , asBDeque
    , DList
    , asDList
      -- * Type classes
    , MutableContainer (..)
    , MutableRef (..)
    , MutableAtomicRef (..)
    , MutableCollection (..)
    , MutablePushFront (..)
    , MutablePushBack (..)
    , MutablePopFront (..)
    , MutablePopBack (..)
      -- * Constraint kinds
    , MutableQueue
    , MutableStack
    , MutableDeque
      -- * Convenience re-exports
    , PrimMonad
    , PrimState
    , RealWorld
    , Prim
    , Unbox
    , Storable
    ) where

import Data.Mutable.Class
import Data.Mutable.URef
import Data.Mutable.SRef
import Data.Mutable.PRef
import Data.Mutable.BRef
import Data.Mutable.Deque
import Data.Mutable.DList
import Data.Vector.Unboxed (Unbox)
import Data.Primitive (Prim)
import Data.Vector.Storable (Storable)
