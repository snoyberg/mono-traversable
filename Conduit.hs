{-# LANGUAGE CPP #-}
-- | Your intended one-stop-shop for conduit functionality.
-- This re-exports functions from many commonly used modules.
-- When there is a conflict with standard functions, functions
-- in this module are disambiguated by adding a trailing C
-- (or for chunked functions, replacing a trailing E with EC).
-- This means that the Conduit module can be imported unqualified
-- without causing naming conflicts.
module Conduit
    ( module X
    ) where

import Data.Conduit as X
import Data.Conduit.Util as X hiding (zip)
import qualified Data.Conduit.Combinators as CC
import Control.Monad.IO.Class as X (MonadIO (..))
import Control.Monad.Trans.Class as X (MonadTrans (..))
import Data.Conduit.Combinators.Unqualified as X
#if MIN_VERSION_conduit(1, 0, 11)
import Data.Conduit.Lift as X
#endif