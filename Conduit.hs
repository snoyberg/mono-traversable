{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Your intended one-stop-shop for conduit functionality.
-- This re-exports functions from many commonly used modules.
-- When there is a conflict with standard functions, functions
-- in this module are disambiguated by adding a trailing C
-- (or for chunked functions, replacing a trailing E with CE).
-- This means that the Conduit module can be imported unqualified
-- without causing naming conflicts.
--
-- For more information on the naming scheme and intended usages of the
-- combinators, please see the "Data.Conduit.Combinators" documentation.
module Conduit
    ( -- * Core conduit library
      module Data.Conduit
#if !MIN_VERSION_conduit(1,1,0)
    , module Data.Conduit.Util
#endif
#if MIN_VERSION_conduit(1, 0, 11)
    , module Data.Conduit.Lift
#endif
      -- * Commonly used combinators
    , module Data.Conduit.Combinators.Unqualified
      -- * Monadic lifting
    , MonadIO (..)
    , MonadTrans (..)
    , MonadBase (..)
    , MonadThrow (..)
    , MonadBaseControl
      -- * ResourceT
    , MonadResource
    , runResourceT
      -- * Acquire
#if MIN_VERSION_resourcet(1,1,0)
    , module Data.Acquire
    , withAcquire
#endif
      -- * Pure pipelines
    , Identity (..)
    ) where

import Data.Conduit
#if !MIN_VERSION_conduit(1,1,0)
import Data.Conduit.Util hiding (zip)
#endif
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Base (MonadBase (..))
#if MIN_VERSION_conduit(1, 0, 11)
import Data.Conduit.Lift
#endif
import Data.Conduit.Combinators.Unqualified
import Data.Functor.Identity (Identity (..))
import Control.Monad.Trans.Resource (MonadResource, MonadThrow (..), runResourceT)
#if MIN_VERSION_resourcet(1,1,0)
import Data.Acquire hiding (with)
import qualified Data.Acquire

withAcquire :: MonadBaseControl IO m => Acquire a -> (a -> m b) -> m b
withAcquire = Data.Acquire.with
#endif
