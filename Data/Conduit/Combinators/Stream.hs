{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
-- | These are streaming versions of some of the functions in
--   "Data.Conduit.Combinators".  Many functions don't have stream
--   versions here because instead they have @RULES@ which inline a
--   definition that fuses.
module Data.Conduit.Combinators.Stream where

-- BEGIN IMPORTS

import           Control.Monad (liftM)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Conduit.Internal.Fusion
import           Data.IOData
import           Data.Maybe (isNothing, isJust)
import           Data.MonoTraversable
import           Data.Monoid (Monoid (..))
import qualified Data.NonNull as NonNull
import qualified Data.Sequences as Seq
import           Prelude
import           System.IO (Handle)

-- END IMPORTS

yieldManyS :: (Monad m, MonoFoldable mono)
            => mono
            -> StreamProducer m (Element mono)
yieldManyS mono _ =
    Stream (return . step) (return (otoList mono))
  where
    step [] = Stop ()
    step (x:xs) = Emit xs x
{-# INLINE yieldManyS #-}

repeatMS :: Monad m
         => m a
         -> StreamProducer m a
repeatMS m _ =
    Stream step (return ())
  where
    step _ = liftM (Emit ()) m
{-# INLINE repeatMS #-}

repeatWhileMS :: Monad m
              => m a
              -> (a -> Bool)
              -> StreamProducer m a
repeatWhileMS m f _ =
    Stream step (return ())
  where
    step _ = do
        x <- m
        return $ if f x
            then Emit () x
            else Stop ()
{-# INLINE repeatWhileMS #-}

sourceHandleS :: (MonadIO m, IOData a) => Handle -> StreamProducer m a
sourceHandleS h _ =
    Stream step (return ())
  where
    step () = do
        x <- liftIO (hGetChunk h)
        return $ if onull x
            then Stop ()
            else Emit () x
{-# INLINE sourceHandleS #-}

foldl1S :: Monad m
        => (a -> a -> a)
        -> StreamConsumer a m (Maybe a)
foldl1S f (Stream step ms0) =
    Stream step' (liftM (Nothing, ) ms0)
  where
    step' (mprev, s) = do
        res <- step s
        return $ case res of
            Stop () -> Stop mprev
            Skip s' -> Skip (mprev, s')
            Emit s' a -> Skip (Just $ maybe a (f a) mprev, s')
{-# INLINE foldl1S #-}

allS :: Monad m
     => (a -> Bool)
     -> StreamConsumer a m Bool
allS f = fmapS isNothing (findS (Prelude.not . f))
{-# INLINE allS #-}

anyS :: Monad m
     => (a -> Bool)
     -> StreamConsumer a m Bool
anyS f = fmapS isJust (findS f)
{-# INLINE anyS #-}

-- Utility function for allS / anyS

fmapS :: Monad m
      => (a -> b)
      -> StreamConduitM i o m a
      -> StreamConduitM i o m b
fmapS f s inp =
    case s inp of
        Stream step ms0 -> Stream (fmap (liftM (fmap f)) step) ms0

--TODO: use a definition like
-- fmapS (fromChunks . ($ [])) <$> CL.fold (\front next -> front . (next:)) id

{-sinkLazyS :: (Monad m, LazySequence lazy strict)
          => StreamConsumer strict m lazy-}

lastS :: Monad m
      => StreamConsumer a m (Maybe a)
lastS (Stream step ms0) =
    Stream step' ms0
  where
    step' s = do
        res <- step s
        return $ case res of
            Stop () -> Stop Nothing
            Skip s' -> Skip s'
            Emit _ x -> Stop (Just x)
{-# INLINE lastS #-}

lastES :: (Monad m, Seq.IsSequence seq)
       => StreamConsumer seq m (Maybe (Element seq))
lastES (Stream step ms0) =
    Stream step' (liftM (Nothing, ) ms0)
  where
    step' (mlast, s) = do
        res <- step s
        return $ case res of
            Stop () -> Stop (fmap NonNull.last mlast)
            Skip s' -> Skip (mlast, s')
            Emit s' (NonNull.fromNullable -> mlast'@(Just _)) -> Skip (mlast', s')
            Emit s' _ -> Skip (mlast, s')
{-# INLINE lastES #-}

findS :: Monad m
      => (a -> Bool) -> StreamConsumer a m (Maybe a)
findS f (Stream step ms0) =
    Stream step' ms0
  where
    step' s = do
      res <- step s
      return $ case res of
          Stop () -> Stop Nothing
          Skip s' -> Skip s'
          Emit s' x ->
              if f x
                  then Stop (Just x)
                  else Skip s'
{-# INLINE findS #-}

concatMapS :: (Monad m, MonoFoldable mono)
           => (a -> mono)
           -> StreamConduit a m (Element mono)
concatMapS f (Stream step ms0) =
    Stream step' (liftM ([], ) ms0)
  where
    step' ([], s) = do
        res <- step s
        return $ case res of
            Stop () -> Stop ()
            Skip s' -> Skip ([], s')
            Emit s' x -> Skip (otoList (f x), s')
    step' ((x:xs), s) = return (Emit (xs, s) x)
{-# INLINE concatMapS #-}

concatMapMS :: (Monad m, MonoFoldable mono)
             => (a -> m mono)
             -> StreamConduit a m (Element mono)
concatMapMS f (Stream step ms0) =
    Stream step' (liftM ([], ) ms0)
  where
    step' ([], s) = do
        res <- step s
        case res of
            Stop () -> return $ Stop ()
            Skip s' -> return $ Skip ([], s')
            Emit s' x -> do
                o <- f x
                return $ Skip (otoList o, s')
    step' ((x:xs), s) = return (Emit (xs, s) x)
{-# INLINE concatMapMS #-}

concatS :: (Monad m, MonoFoldable mono)
         => StreamConduit mono m (Element mono)
concatS = concatMapS id
{-# INLINE concatS #-}

scanlS :: Monad m => (a -> b -> a) -> a -> StreamConduit b m a
scanlS f seed0 (Stream step ms0) =
    Stream step' (liftM (seed0, ) ms0)
  where
    step' (seed, s) = do
        res <- step s
        return $ case res of
            Stop () -> Stop ()
            Skip s' -> Skip (seed, s')
            Emit s' x -> Emit (r, s') r
              where
                !r = f seed x
{-# INLINE scanlS #-}

scanlMS :: Monad m => (a -> b -> m a) -> a -> StreamConduit b m a
scanlMS f seed0 (Stream step ms0) =
    Stream step' (liftM (seed0, ) ms0)
  where
    step' (seed, s) = do
        res <- step s
        case res of
            Stop () -> return $ Stop ()
            Skip s' -> return $ Skip (seed, s')
            Emit s' x -> do
                !r <- f seed x
                return $ Emit (r, s') r
{-# INLINE scanlMS #-}

intersperseS :: Monad m => a -> StreamConduit a m a
intersperseS sep (Stream step ms0) =
    Stream step' (liftM (False, ) ms0)
  where
    -- Emit a separator
    step' (True, s) = return $ Emit (False, s) sep
    -- Wait for a value
    step' (False, s) = do
        res <- step s
        return $ case res of
            Stop () -> Stop ()
            Skip s' -> Skip (False, s')
            Emit s' x -> Emit (True, s') x
{-# INLINE intersperseS #-}

slidingWindowS :: (Monad m, Seq.IsSequence seq, Element seq ~ a) => Int -> StreamConduit a m seq
slidingWindowS sz (Stream step ms0) =
    Stream step' (liftM (Just (max 1 sz), mempty, ) ms0)
  where
    step' (mn, st, s) = do
        res <- step s
        return $ case res of
            Stop () -> Stop ()
            Skip s' -> Skip (mn, st, s')
            Emit s' x ->
                case mn of
                     -- Collecting the initial window (a prefix of the
                     -- input stream)
                     Just 1 -> Emit (Nothing, Seq.unsafeTail st', s') st'
                     Just n -> Skip (Just (n - 1), st', s')
                     -- After collecting the initial window, each
                     -- upstream element causes an additional window
                     -- to be yielded.
                     Nothing -> Emit (Nothing, Seq.unsafeTail st', s') st'
               where
                 st' = Seq.snoc st x
{-# INLINE slidingWindowS #-}

filterMS :: Monad m
         => (a -> m Bool)
         -> StreamConduit a m a
filterMS f (Stream step ms0) = do
    Stream step' ms0
  where
    step' s = do
        res <- step s
        case res of
            Stop () -> return $ Stop ()
            Skip s' -> return $ Skip s'
            Emit s' x -> do
                r <- f x
                return $
                    if r
                        then Emit s' x
                        else Skip s'
{-# INLINE filterMS #-}

data SplitState seq s
    = SplitDone
    -- When no element of seq passes the predicate.  This allows
    -- 'splitOnUnboundedES' to not run 'Seq.break' multiple times due
    -- to 'Skip's being sent by the upstream.
    | SplitNoSep seq s
    | SplitState seq s

splitOnUnboundedES :: (Monad m, Seq.IsSequence seq)
                   => (Element seq -> Bool) -> StreamConduit seq m seq
splitOnUnboundedES f (Stream step ms0) =
    Stream step' (liftM (SplitState mempty) ms0)
  where
    step' SplitDone = return $ Stop ()
    step' (SplitNoSep t s) = do
        res <- step s
        return $ case res of
            Stop () | not (onull t) -> Emit SplitDone t
                    | otherwise -> Stop ()
            Skip s' -> Skip (SplitNoSep t s')
            Emit s' t' -> Skip (SplitState (t `mappend` t') s')
    step' (SplitState t s) = do
        if onull y
            then do
                res <- step s
                return $ case res of
                    Stop () | not (onull t) -> Emit SplitDone t
                            | otherwise -> Stop ()
                    Skip s' -> Skip (SplitNoSep t s')
                    Emit s' t' -> Skip (SplitState (t `mappend` t') s')
            else return $ Emit (SplitState (Seq.drop 1 y) s) x
      where
        (x, y) = Seq.break f t
{-# INLINE splitOnUnboundedES #-}
