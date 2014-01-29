{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- | This module is meant as a replacement for Data.Conduit.List.
-- That module follows a naming scheme which was originally inspired
-- by its enumerator roots. This module is meant to introduce a naming
-- scheme which encourages conduit best practices.
--
-- There are two versions of functions in this module. Those with a trailing
-- E work in the individual elements of a chunk of data, e.g., the bytes of
-- a ByteString, the Chars of a Text, or the Ints of a Vector Int. Those
-- without a trailing E work on unchunked streams.
--
-- FIXME: discuss overall naming, usage of mono-traversable, etc
--
-- Mention take (Conduit) vs drop (Consumer)
module Data.Conduit.Combinators
    ( -- * Producers
      -- ** Pure
      yieldMany
    , unfold
    , enumFromTo
    , iterate
    , repeat
    , replicate
    , sourceLazy

      -- ** Monadic
    , repeatM
    , repeatWhileM
    , replicateM

      -- ** I\/O
    , sourceFile
    , sourceHandle
    , sourceIOHandle

      -- * Consumers
      -- ** Pure
    , drop
    , dropE
    , dropWhile
    , dropWhileE
-- FIXME need to organized/document below this point.
    , fold
    , foldl
    , foldE
    , foldlE
    , CL.foldMap
    , foldMapE
    , all
    , allE
    , any
    , anyE
    , and
    , andE
    , or
    , orE
    , elem
    , elemE
    , notElem
    , notElemE
    , sinkLazy
    , sinkList
    , sinkVector
    , CL.sinkNull

      -- ** Monadic
    , CL.mapM_
    , mapM_E
    , CL.foldM
    , foldME
    , CL.foldMapM
    , foldMapME

      -- ** I\/O
    , sinkFile
    , sinkHandle

      -- * Transformers
      -- ** Pure
    , CL.map
    , mapE
    , omapE
    , concatMap
    , concatMapE
    , take
    , takeE
    , takeWhile
    , takeWhileE
    , takeExactly
    , takeExactlyE
    , concat
    , CL.filter
    , filterE
    , mapWhile

      -- ** Monadic
    , CL.mapM
    , mapME
    , omapME
    , concatMapM
    , filterM
    , filterME
    , CL.iterM
    ) where

import qualified Data.Traversable
import           Control.Applicative         ((<$>))
import           Control.Category            (Category (..))
import           Control.Monad               (unless, when, (>=>), liftM, forever)
import           Control.Monad.Base          (MonadBase (liftBase))
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.Primitive     (PrimMonad)
import           Control.Monad.Trans.Class   (lift)
import           Data.Conduit
import qualified Data.Conduit.List           as CL
import           Data.IOData
import           Data.Monoid                 (Monoid (..))
import           Data.MonoTraversable
import qualified Data.Sequences              as Seq
import           Data.Sequences.Lazy
import qualified Data.Vector.Generic         as V
import qualified Data.Vector.Generic.Mutable as VM
import qualified Filesystem                  as F
import           Filesystem.Path             (FilePath)
import           Prelude                     (Bool (..), Eq (..), Int,
                                              Maybe (..), Monad (..), Num (..),
                                              Ord (..), fromIntegral, maybe,
                                              otherwise, ($), Functor (..))
import           System.IO                   (Handle)
import qualified System.IO                   as SIO

-- | Yield each of the values contained by the given @MonoFoldable@.
--
-- This will work on many data structures, including lists, @ByteString@s, and @Vector@s.
--
-- Since 1.0.0
yieldMany :: (Monad m, MonoFoldable mono)
          => mono
          -> Producer m (Element mono)
yieldMany = ofoldMap yield
{-# INLINE yieldMany #-}

-- | Generate a producer from a seed value.
--
-- Since 1.0.0
unfold :: Monad m
       => (b -> Maybe (a, b))
       -> b
       -> Producer m a
unfold = CL.unfold
{-# INLINE unfold #-}

-- | Enumerate from a value to a final value, inclusive, via 'succ'.
--
-- This is generally more efficient than using @Prelude@\'s @enumFromTo@ and
-- combining with @sourceList@ since this avoids any intermediate data
-- structures.
--
-- Since 1.0.0
enumFromTo = CL.enumFromTo

-- | Produces an infinite stream of repeated applications of f to x.
--
-- Since 1.0.0
iterate = CL.iterate
{-# INLINE iterate #-}

-- | Produce an infinite stream consisting entirely of the given value.
--
-- Since 1.0.0
repeat = iterate id
{-# INLINE repeat #-}

-- | Produce a finite stream consistent of n copies of the given value.
--
-- Since 1.0.0
replicate :: Monad m
          => Int
          -> a
          -> Producer m a
replicate count0 a =
    loop count0
  where
    loop count
        | count <= 0 = return ()
        | otherwise = yield a >> loop (count - 1)
{-# INLINE replicate #-}

-- | Generate a producer by yielding each of the strict chunks in a @LazySequence@.
--
-- For more information, see 'toChunks'.
--
-- Since 1.0.0
sourceLazy :: (Monad m, LazySequence lazy strict)
           => lazy
           -> Producer m strict
sourceLazy = yieldMany . toChunks
{-# INLINE sourceLazy #-}

-- | Repeatedly run the given action and yield all values it produces.
--
-- Since 1.0.0
repeatM :: Monad m
        => m a
        -> Producer m a
repeatM m = forever $ lift m >>= yield
{-# INLINE repeatM #-}

-- | Repeatedly run the given action and yield all values it produces, until
-- the provided predicate returns @False@.
--
-- Since 1.0.0
repeatWhileM :: Monad m
             => m a
             -> (a -> Bool)
             -> Producer m a
repeatWhileM m f =
    loop
  where
    loop = do
        x <- lift m
        when (f x) $ yield x >> loop

-- | Perform the given action n times, yielding each result.
--
-- Since 1.0.0
replicateM :: Monad m
           => Int
           -> m a
           -> Producer m a
replicateM count0 m =
    loop count0
  where
    loop count
        | count <= 0 = return ()
        | otherwise = lift m >>= yield >> loop (count - 1)
{-# INLINE replicateM #-}

-- | Read all data from the given file.
--
-- This function automatically opens and closes the file handle, and ensures
-- exception safety via @MonadResource. It works for all instances of @IOData@,
-- including @ByteString@ and @Text@.
--
-- Since 1.0.0
sourceFile :: (MonadResource m, IOData a) => FilePath -> Producer m a
sourceFile fp = sourceIOHandle (F.openFile fp SIO.ReadMode)
{-# INLINE sourceFile #-}

-- | Read all data from the given @Handle@.
--
-- Does not close the @Handle@ at any point.
--
-- Since 1.0.0
sourceHandle :: (MonadIO m, IOData a) => Handle -> Producer m a
sourceHandle h =
    loop
  where
    loop = do
        x <- liftIO (hGetChunk h)
        if onull x
            then return ()
            else yield x >> loop
{-# INLINEABLE sourceHandle #-}

-- | Open a @Handle@ using the given function and stream data from it.
--
-- Automatically closes the file at completion.
--
-- Since 1.0.0
sourceIOHandle :: (MonadResource m, IOData a) => SIO.IO Handle -> Producer m a
sourceIOHandle alloc = bracketP alloc SIO.hClose sourceHandle
{-# INLINE sourceIOHandle #-}

sinkLazy :: (Monad m, LazySequence lazy strict)
         => Consumer strict m lazy
sinkLazy = (fromChunks . ($ [])) <$> CL.fold (\front next -> front . (next:)) id
{-# INLINE sinkLazy #-}

sinkList = CL.consume
{-# INLINE sinkList #-}

-- | Sink incoming values into a vector, up until size @maxSize@.
-- Subsequent values will be left in the stream.
sinkVector :: (MonadBase base m, V.Vector v a, PrimMonad base)
           => Int -- ^ maximum allowed size
           -> Consumer a m (v a)
sinkVector maxSize = do
    mv <- liftBase $ VM.new maxSize
    let go i
            | i >= maxSize = liftBase $ V.unsafeFreeze mv
            | otherwise = do
                mx <- await
                case mx of
                    Nothing -> V.slice 0 i <$> liftBase (V.unsafeFreeze mv)
                    Just x -> do
                        liftBase $ VM.write mv i x
                        go (i + 1)
    go 0
{-# INLINEABLE sinkVector #-}

sinkHandle :: (MonadIO m, IOData a) => Handle -> Consumer a m ()
sinkHandle = CL.mapM_ . hPut
{-# INLINE sinkHandle #-}

sinkIOHandle :: (MonadResource m, IOData a) => SIO.IO Handle -> Consumer a m ()
sinkIOHandle alloc = bracketP alloc SIO.hClose sinkHandle
{-# INLINE sinkIOHandle #-}

-- | Ignore a certain number of values in the stream.
--
-- Since 1.0.0
drop :: Monad m
     => Int
     -> Consumer a m ()
drop = CL.drop
{-# INLINE drop #-}

-- | Drop a certain number of elements from a chunked stream.
--
-- Since 1.0.0
dropE :: (Monad m, Seq.IsSequence seq)
      => Seq.Index seq
      -> Consumer seq m ()
dropE =
    loop
  where
    loop i
        | i <= 0 = return ()
        | otherwise = await >>= maybe (return ()) (go i)

    go i seq = do
        unless (onull y) $ leftover y
        loop i'
      where
        (x, y) = Seq.splitAt i seq
        i' = i - fromIntegral (olength x)
{-# INLINEABLE dropE #-}

-- | Drop all values which match the given predicate.
--
-- Since 1.0.0
dropWhile :: Monad m
          => (a -> Bool)
          -> Consumer a m ()
dropWhile f =
    loop
  where
    loop = await >>= maybe (return ()) go
    go x
        | f x = loop
        | otherwise = leftover x
{-# INLINE dropWhile #-}

-- FIXME need to organized/document below this point.

-- | Drop all elements in the chunked stream which match the given predicate.
--
-- Since 1.0.0
dropWhileE :: (Monad m, Seq.IsSequence seq)
           => (Element seq -> Bool)
           -> Consumer seq m ()
dropWhileE f =
    loop
  where
    loop = await >>= maybe (return ()) go

    go seq
        | onull x = loop
        | otherwise = leftover x
      where
        x = Seq.dropWhile f seq
{-# INLINE dropWhileE #-}

sinkFile :: (MonadResource m, IOData a) => FilePath -> Consumer a m ()
sinkFile fp = sinkIOHandle (F.openFile fp SIO.WriteMode)
{-# INLINE sinkFile #-}

-- | Generalizes concatMap, mapMaybe, mapFoldable
concatMap :: (Monad m, MonoFoldable mono)
          => (a -> mono)
          -> Conduit a m (Element mono)
concatMap f = awaitForever (yieldMany . f)
{-# INLINE concatMap #-}

concatMapE :: (Monad m, MonoFoldable mono, Monoid w)
           => (Element mono -> w)
           -> Conduit mono m w
concatMapE = CL.map . ofoldMap
{-# INLINE concatMapE #-}

-- | Generalizes concatMapM, mapMaybeM, mapFoldableM
concatMapM :: (Monad m, MonoFoldable mono)
           => (a -> m mono)
           -> Conduit a m (Element mono)
concatMapM f = awaitForever (lift . f >=> yieldMany)
{-# INLINE concatMapM #-}

take :: Monad m => Int -> Conduit a m a
take =
    loop
  where
    loop count
        | count <= 0 = return ()
        | otherwise = await >>= maybe (return ()) (\i -> yield i >> loop (count - 1))
{-# INLINE take #-}

takeExactly :: Monad m
            => Int
            -> ConduitM a b m r
            -> ConduitM a b m r
takeExactly count inner = take count =$= do
    r <- inner
    CL.sinkNull
    return r
{-# INLINE takeExactly #-}

takeExactlyE :: (Monad m, Seq.IsSequence a)
             => Seq.Index a
             -> ConduitM a b m r
             -> ConduitM a b m r
takeExactlyE count inner = takeE count =$= do
    r <- inner
    CL.sinkNull
    return r
{-# INLINE takeExactlyE #-}

takeWhile :: Monad m
          => (a -> Bool)
          -> Conduit a m a
takeWhile f =
    loop
  where
    loop = await >>= maybe (return ()) go
    go x
        | f x = yield x >> loop
        | otherwise = leftover x
{-# INLINE takeWhile #-}

concat :: (Monad m, MonoFoldable mono)
       => Conduit mono m (Element mono)
concat = awaitForever yieldMany
{-# INLINE concat #-}

filterM :: Monad m
        => (a -> m Bool)
        -> Conduit a m a
filterM f =
    awaitForever go
  where
    go x = do
        b <- lift $ f x
        when b $ yield x
{-# INLINE filterM #-}

all :: Monad m
    => (a -> Bool)
    -> Consumer a m Bool
all f =
    loop
  where
    loop = await >>= maybe (return True) go
    go x
        | f x = loop
        | otherwise = return False
{-# INLINE all #-}

any :: Monad m
    => (a -> Bool)
    -> Consumer a m Bool
any f =
    loop
  where
    loop = await >>= maybe (return False) go
    go x
        | f x = return True
        | otherwise = loop
{-# INLINE any #-}

and :: Monad m => Consumer Bool m Bool
and = all id
{-# INLINE and #-}

or :: Monad m => Consumer Bool m Bool
or = any id
{-# INLINE or #-}

elem :: (Monad m, Eq a) => a -> Consumer a m Bool
elem x = any (== x)
{-# INLINE elem #-}

notElem :: (Monad m, Eq a) => a -> Consumer a m Bool
notElem x = all (/= x)
{-# INLINE notElem #-}

-- | Map values as long as the result is @Just@.
mapWhile :: Monad m => (a -> Maybe b) -> Conduit a m b
mapWhile f =
    loop
  where
    loop = await >>= maybe (return ()) go
    go x =
        case f x of
            Just y -> yield y >> loop
            Nothing -> leftover x

takeWhileE :: (Monad m, Seq.IsSequence seq)
           => (Element seq -> Bool)
           -> Conduit seq m seq
takeWhileE f =
    loop
  where
    loop = await >>= maybe (return ()) go

    go seq = do
        unless (onull x) $ yield x
        if onull y
            then loop
            else leftover y
      where
        (x, y) = Seq.span f seq
{-# INLINE takeWhileE #-}

takeE :: (Monad m, Seq.IsSequence seq)
      => Seq.Index seq
      -> Conduit seq m seq
takeE =
    loop
  where
    loop i
        | i <= 0 = return ()
        | otherwise = await >>= maybe (return ()) (go i)

    go i seq = do
        unless (onull x) $ yield x
        unless (onull y) $ leftover y
        loop i'
      where
        (x, y) = Seq.splitAt i seq
        i' = i - fromIntegral (olength x)
{-# INLINEABLE takeE #-}

fold :: (Monad m, Monoid a)
     => Consumer a m a
fold = CL.foldMap id
{-# INLINE fold #-}

foldE :: (Monad m, MonoFoldable mono, Monoid (Element mono))
      => Consumer mono m (Element mono)
foldE = CL.fold (\accum mono -> accum `mappend` ofoldMap id mono) mempty
{-# INLINE foldE #-}

foldl = CL.fold
{-# INLINE foldl #-}

foldlE :: (Monad m, MonoFoldable mono)
       => (a -> Element mono -> a)
       -> a
       -> Consumer mono m a
foldlE f = CL.fold (ofoldl' f)
{-# INLINE foldlE #-}

foldME :: (Monad m, MonoFoldable mono)
       => (a -> Element mono -> m a)
       -> a
       -> Consumer mono m a
foldME f = CL.foldM (ofoldlM f)
{-# INLINE foldME #-}

foldMapE :: (Monad m, MonoFoldable mono, Monoid w)
         => (Element mono -> w)
         -> Consumer mono m w
foldMapE = CL.foldMap . ofoldMap
{-# INLINE foldMapE #-}

foldMapME :: (Monad m, MonoFoldable mono, Monoid w)
          => (Element mono -> m w)
          -> Consumer mono m w
foldMapME f =
    CL.foldM go mempty
  where
    go = ofoldlM (\accum e -> mappend accum `liftM` f e)
{-# INLINE foldMapME #-}

allE :: (Monad m, MonoFoldable mono)
     => (Element mono -> Bool)
     -> Consumer mono m Bool
allE = all . oall

anyE :: (Monad m, MonoFoldable mono)
     => (Element mono -> Bool)
     -> Consumer mono m Bool
anyE = any . oany

andE :: (Monad m, MonoFoldable mono, Element mono ~ Bool)
     => Consumer mono m Bool
andE = allE id

orE :: (Monad m, MonoFoldable mono, Element mono ~ Bool)
    => Consumer mono m Bool
orE  = anyE id

elemE :: (Monad m, Seq.EqSequence seq)
      => Element seq
      -> Consumer seq m Bool
elemE = any . Seq.elem

notElemE :: (Monad m, Seq.EqSequence seq)
         => Element seq
         -> Consumer seq m Bool
notElemE = all . Seq.notElem

mapM_E = CL.mapM_ . omapM_

mapE = CL.map . fmap
{-# INLINE mapE #-}

mapME = CL.mapM . Data.Traversable.mapM
{-# INLINE mapME #-}

omapME = CL.mapM . omapM
{-# INLINE omapME #-}

omapE = CL.map . omap
{-# INLINE omapE #-}

filterE = CL.map . Seq.filter
{-# INLINE filterE #-}

filterME = CL.mapM . Seq.filterM
{-# INLINE filterME #-}

-- | Apply a monadic action on all values in a stream.
--
-- This @Conduit@ can be used to perform a monadic side-effect for every
-- value, whilst passing the value through the @Conduit@ as-is.
--
-- > iterM f = mapM (\a -> f a >>= \() -> return a)
--
-- Since 0.5.6
iterM = CL.iterM

{-

-- FIXME

lines
unlines
peek
concatMapAccum
scanl
groupBy
scanlM
concatMapAccumM
encode
decode
withLine
print
find
last
length
maximum
minimum
null
sum
product
omap
omapM
foldLines
intercalate
split
splitWith

-}
