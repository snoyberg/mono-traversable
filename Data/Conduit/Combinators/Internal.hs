{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
-- | Internal helper functions, usually used for rewrite rules.
module Data.Conduit.Combinators.Internal
    ( initReplicate
    , initReplicateConnect
    , initRepeat
    , initRepeatConnect
    ) where

import Data.Conduit
import Data.Conduit.Internal (ConduitM (..), Pipe (..), injectLeftovers)
import Data.Void (absurd)
import Control.Monad.Trans.Class (lift)
import Control.Monad (replicateM_, forever)
import Data.Conduit.Combinators.Stream
import Data.Conduit.Internal.Fusion

-- Defines INLINE_RULE0, INLINE_RULE, STREAMING0, and STREAMING.
#include "fusion-macros.h"

-- | Acquire the seed value and perform the given action with it n times,
-- yielding each result.
--
-- Subject to fusion
--
-- Since 0.2.1
initReplicate, initReplicateC :: Monad m => m seed -> (seed -> m a) -> Int -> Producer m a
initReplicateC mseed f cnt = do
    seed <- lift mseed
    replicateM_ cnt (lift (f seed) >>= yield)
{-# INLINE [1] initReplicateC #-}
STREAMING(initReplicate, mseed f cnt)

-- | Optimized version of initReplicate for the special case of connecting with
-- a @Sink@.
--
-- Since 0.2.1
initReplicateConnect :: Monad m
                     => m seed
                     -> (seed -> m a)
                     -> Int
                     -> Sink a m b
                     -> m b
initReplicateConnect mseed f cnt0 (ConduitM sink0) = do
    seed <- mseed
    let loop cnt sink | cnt <= 0 = finish sink
        loop _ (Done r) = return r
        loop cnt (NeedInput p _) = f seed >>= loop (pred cnt) . p
        loop _ (HaveOutput _ _ o) = absurd o
        loop cnt (PipeM mp) = mp >>= loop cnt
        loop _ (Leftover _ i) = absurd i

#if MIN_VERSION_conduit(1, 2, 0)
    loop cnt0 (injectLeftovers $ sink0 Done)
#else
    loop cnt0 (injectLeftovers sink0)
#endif
  where
    finish (Done r) = return r
    finish (HaveOutput _ _ o) = absurd o
    finish (NeedInput _ p) = finish (p ())
    finish (PipeM mp) = mp >>= finish
    finish (Leftover _ i) = absurd i
{-# RULES "initReplicateConnect" forall mseed f cnt sink.
    initReplicate mseed f cnt $$ sink
    = initReplicateConnect mseed f cnt sink
  #-}

-- | Acquire the seed value and perform the given action with it forever,
-- yielding each result.
--
-- Subject to fusion
--
-- Since 0.2.1
initRepeat, initRepeatC :: Monad m => m seed -> (seed -> m a) -> Producer m a
initRepeatC mseed f = do
    seed <- lift mseed
    forever $ lift (f seed) >>= yield
{-# INLINE [1] initRepeatC #-}
STREAMING(initRepeat, mseed f)

-- | Optimized version of initRepeat for the special case of connecting with
-- a @Sink@.
--
-- Since 0.2.1
initRepeatConnect :: Monad m
                  => m seed
                  -> (seed -> m a)
                  -> Sink a m b
                  -> m b
initRepeatConnect mseed f (ConduitM sink0) = do
    seed <- mseed
    let loop (Done r) = return r
        loop (NeedInput p _) = f seed >>= loop . p
        loop (HaveOutput _ _ o) = absurd o
        loop (PipeM mp) = mp >>= loop
        loop (Leftover _ i) = absurd i

#if MIN_VERSION_conduit(1, 2, 0)
    loop (injectLeftovers (sink0 Done))
#else
    loop (injectLeftovers sink0)
#endif
{-# RULES "initRepeatConnect" forall mseed f sink.
    initRepeat mseed f $$ sink
    = initRepeatConnect mseed f sink
  #-}
