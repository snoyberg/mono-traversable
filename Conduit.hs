{-# LANGUAGE CPP #-}
-- | Your intended one-stop-shop for conduit functionality.
-- This re-exports functions from many commonly used modules.
-- When there is a conflict with standard functions, functions
-- in this module are disambiguated by adding a trailing C
-- (or for chunked functions, replacing a trailing E with EC).
-- This means that the Conduit module can be imported unqualified
-- without causing naming conflicts.
module Conduit
    ( -- * Core conduit library
      module Data.Conduit
    , module Data.Conduit.Util
#if MIN_VERSION_conduit(1, 0, 11)
    , module Data.Conduit.Lift
#endif
      -- * Commonly used combinators
    , module Data.Conduit.Combinators.Unqualified
      -- * Monadic lifting
    , MonadIO (..)
    , MonadTrans (..)
    , MonadBase (..)
    ) where

import Data.Conduit
import Data.Conduit.Util hiding (zip)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Base (MonadBase (..))
#if MIN_VERSION_conduit(1, 0, 11)
import Data.Conduit.Lift
#endif
import Data.Conduit.Combinators.Unqualified
