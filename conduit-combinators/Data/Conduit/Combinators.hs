{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE BangPatterns #-}
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
    , stdin

      -- ** Random numbers
    , sourceRandom
    , sourceRandomN
    , sourceRandomGen
    , sourceRandomNGen
    , sourceRandomWith
    , sourceRandomNWith
    , sourceRandomGenWith
    , sourceRandomNGenWith

      -- ** Filesystem
    , sourceDirectory
    , sourceDirectoryDeep

      -- * Consumers
      -- ** Pure
    , drop
    , dropE
    , dropWhile
    , dropWhileE
    , fold
    , foldE
    , foldl
    , foldl1
    , foldlE
    , foldMap
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
    , sinkVectorN
    , sinkBuilder
    , sinkLazyBuilder
    , sinkNull
    , awaitNonNull
    , headE
    , peek
    , peekE
    , last
    , lastE
    , length
    , lengthE
    , lengthIf
    , lengthIfE
    , maximum
    , maximumE
    , minimum
    , minimumE
    , null
    , nullE
    , sum
    , sumE
    , product
    , productE
    , find

      -- ** Monadic
    , mapM_
    , mapM_E
    , foldM
    , foldME
    , foldMapM
    , foldMapME

      -- ** I\/O
    , sinkFile
    , sinkHandle
    , sinkIOHandle
    , print
    , stdout
    , stderr

      -- * Transformers
      -- ** Pure
    , map
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
    , filter
    , filterE
    , mapWhile
    , conduitVector
    , scanl
    , mapAccumWhile
    , concatMapAccum
    , intersperse
    , slidingWindow

      -- *** Binary base encoding
    , encodeBase64
    , decodeBase64
    , encodeBase64URL
    , decodeBase64URL
    , encodeBase16
    , decodeBase16

      -- ** Monadic
    , mapM
    , mapME
    , omapME
    , concatMapM
    , filterM
    , filterME
    , iterM
    , scanlM
    , mapAccumWhileM
    , concatMapAccumM

      -- ** Textual
    , encodeUtf8
    , decodeUtf8
    , decodeUtf8Lenient
    , line
    , lineAscii
    , unlines
    , unlinesAscii
    , takeExactlyUntilE
    , linesUnbounded
    , linesUnboundedAscii
    , splitOnUnboundedE

      -- * Special
    , vectorBuilder
    , mapAccumS
    , peekForever
    ) where

-- BEGIN IMPORTS

import Data.Builder
import qualified Data.NonNull as NonNull
import qualified Data.Traversable
import qualified Data.ByteString as S
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64U
import           Control.Applicative         ((<$>))
import           Control.Exception           (assert)
import           Control.Category            (Category (..))
import           Control.Monad               (unless, when, (>=>), liftM, forever)
import           Control.Monad.Base          (MonadBase (liftBase))
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.Primitive     (PrimMonad, PrimState)
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.Resource (MonadResource, MonadThrow)
import           Data.Conduit
import qualified Data.Conduit.Filesystem as CF
import           Data.Conduit.Internal       (ConduitM (..), Pipe (..))
import qualified Data.Conduit.List           as CL
import           Data.IOData
import           Data.Maybe                  (isNothing, isJust)
import           Data.Monoid                 (Monoid (..))
import           Data.MonoTraversable
import qualified Data.Sequences              as Seq
import qualified Data.Vector.Generic         as V
import qualified Data.Vector.Generic.Mutable as VM
import           Data.Void                   (absurd)
import           Prelude                     (Bool (..), Eq (..), Int,
                                              Maybe (..), Either (..), Monad (..), Num (..),
                                              Ord (..), fromIntegral, maybe, either,
                                              ($), Functor (..), Enum, seq, Show, Char,
                                              mod, otherwise, Either (..),
                                              ($!), succ, FilePath)
import Data.Word (Word8)
import qualified Prelude
import           System.IO                   (Handle)
import qualified System.IO                   as SIO
import qualified Data.Conduit.Text as CT
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified System.Random.MWC as MWC
import Data.Conduit.Combinators.Internal
import Data.Conduit.Combinators.Stream
import Data.Conduit.Internal.Fusion
import           Data.Primitive.MutVar       (MutVar, newMutVar, readMutVar,
                                              writeMutVar)

#if MIN_VERSION_mono_traversable(1,0,0)
import qualified Data.Sequences as DTE
import           Data.Sequences (LazySequence (..))
#else
import           Data.Sequences.Lazy
import qualified Data.Textual.Encoding as DTE
#endif

-- Defines INLINE_RULE0, INLINE_RULE, STREAMING0, and STREAMING.
#include "fusion-macros.h"

-- END IMPORTS

-- TODO:
--
--   * The functions sourceRandom* are based on, initReplicate and
--   initRepeat have specialized versions for when they're used with
--   ($$).  How does this interact with stream fusion?
--
--   * Is it possible to implement fusion for vectorBuilder?  Since it
--   takes a Sink yielding function as an input, the rewrite rule
--   would need to trigger when that parameter looks something like
--   (\x -> unstream (...)).  I don't see anything preventing doing
--   this, but it would be quite a bit of code.

-- NOTE: Fusion isn't possible for the following operations:
--
--   * Due to a lack of leftovers:
--     - dropE, dropWhile, dropWhileE
--     - headE
--     - peek, peekE
--     - null, nullE
--     - takeE, takeWhile, takeWhileE
--     - mapWhile
--     - codeWith
--     - line
--     - lineAscii
--
--   * Due to a use of leftover in a dependency:
--     - Due to "codeWith": encodeBase64, decodeBase64, encodeBase64URL, decodeBase64URL, decodeBase16
--     - due to "CT.decode": decodeUtf8, decodeUtf8Lenient
--
--   * Due to lack of resource cleanup (e.g. bracketP):
--     - sourceDirectory
--     - sourceDirectoryDeep
--     - sourceFile
--
--   * takeExactly / takeExactlyE - no monadic bind.  Another way to
--   look at this is that subsequent streams drive stream evaluation,
--   so there's no way for the conduit to guarantee a certain amount
--   of demand from the upstream.

-- | Yield each of the values contained by the given @MonoFoldable@.
--
-- This will work on many data structures, including lists, @ByteString@s, and @Vector@s.
--
-- Subject to fusion
--
-- Since 1.0.0
yieldMany, yieldManyC :: (Monad m, MonoFoldable mono)
                      => mono
                      -> Producer m (Element mono)
yieldManyC = ofoldMap yield
{-# INLINE yieldManyC #-}
STREAMING(yieldMany, yieldManyC, yieldManyS, x)

-- | Generate a producer from a seed value.
--
-- Subject to fusion
--
-- Since 1.0.0
unfold :: Monad m
       => (b -> Maybe (a, b))
       -> b
       -> Producer m a
INLINE_RULE(unfold, f x, CL.unfold f x)

-- | Enumerate from a value to a final value, inclusive, via 'succ'.
--
-- This is generally more efficient than using @Prelude@\'s @enumFromTo@ and
-- combining with @sourceList@ since this avoids any intermediate data
-- structures.
--
-- Subject to fusion
--
-- Since 1.0.0
enumFromTo :: (Monad m, Enum a, Ord a) => a -> a -> Producer m a
INLINE_RULE(enumFromTo, f t, CL.enumFromTo f t)

-- | Produces an infinite stream of repeated applications of f to x.
--
-- Subject to fusion
--
-- Since 1.0.0
iterate :: Monad m => (a -> a) -> a -> Producer m a
INLINE_RULE(iterate, f t, CL.iterate f t)

-- | Produce an infinite stream consisting entirely of the given value.
--
-- Subject to fusion
--
-- Since 1.0.0
repeat :: Monad m => a -> Producer m a
INLINE_RULE(repeat, x, iterate id x)

-- | Produce a finite stream consisting of n copies of the given value.
--
-- Subject to fusion
--
-- Since 1.0.0
replicate :: Monad m
          => Int
          -> a
          -> Producer m a
INLINE_RULE(replicate, n x, CL.replicate n x)

-- | Generate a producer by yielding each of the strict chunks in a @LazySequence@.
--
-- For more information, see 'toChunks'.
--
-- Subject to fusion
--
-- Since 1.0.0
sourceLazy :: (Monad m, LazySequence lazy strict)
           => lazy
           -> Producer m strict
INLINE_RULE(sourceLazy, x, yieldMany (toChunks x))

-- | Repeatedly run the given action and yield all values it produces.
--
-- Subject to fusion
--
-- Since 1.0.0
repeatM, repeatMC :: Monad m
                  => m a
                  -> Producer m a
repeatMC m = forever $ lift m >>= yield
{-# INLINE repeatMC #-}
STREAMING(repeatM, repeatMC, repeatMS, m)

-- | Repeatedly run the given action and yield all values it produces, until
-- the provided predicate returns @False@.
--
-- Subject to fusion
--
-- Since 1.0.0
repeatWhileM, repeatWhileMC :: Monad m
                            => m a
                            -> (a -> Bool)
                            -> Producer m a
repeatWhileMC m f =
    loop
  where
    loop = do
        x <- lift m
        when (f x) $ yield x >> loop
STREAMING(repeatWhileM, repeatWhileMC, repeatWhileMS, m f)

-- | Perform the given action n times, yielding each result.
--
-- Subject to fusion
--
-- Since 1.0.0
replicateM :: Monad m
           => Int
           -> m a
           -> Producer m a
INLINE_RULE(replicateM, n m, CL.replicateM n m)

-- | Read all data from the given file.
--
-- This function automatically opens and closes the file handle, and ensures
-- exception safety via @MonadResource@. It works for all instances of @IOData@,
-- including @ByteString@ and @Text@.
--
-- Since 1.0.0
sourceFile :: (MonadResource m, IOData a, MonoFoldable a) => FilePath -> Producer m a
sourceFile fp = sourceIOHandle (SIO.openFile fp SIO.ReadMode)
{-# INLINE sourceFile #-}

-- | Read all data from the given @Handle@.
--
-- Does not close the @Handle@ at any point.
--
-- Subject to fusion
--
-- Since 1.0.0
sourceHandle, sourceHandleC :: (MonadIO m, IOData a, MonoFoldable a) => Handle -> Producer m a
sourceHandleC h =
    loop
  where
    loop = do
        x <- liftIO (hGetChunk h)
        if onull x
            then return ()
            else yield x >> loop
{-# INLINEABLE sourceHandleC #-}
STREAMING(sourceHandle, sourceHandleC, sourceHandleS, h)

-- | Open a @Handle@ using the given function and stream data from it.
--
-- Automatically closes the file at completion.
--
-- Since 1.0.0
sourceIOHandle :: (MonadResource m, IOData a, MonoFoldable a) => SIO.IO Handle -> Producer m a
sourceIOHandle alloc = bracketP alloc SIO.hClose sourceHandle
{-# INLINE sourceIOHandle #-}

-- | @sourceHandle@ applied to @stdin@.
--
-- Subject to fusion
--
-- Since 1.0.0
stdin :: (MonadIO m, IOData a, MonoFoldable a) => Producer m a
INLINE_RULE0(stdin, sourceHandle SIO.stdin)

-- | Create an infinite stream of random values, seeding from the system random
-- number.
--
-- Subject to fusion
--
-- Since 1.0.0
sourceRandom :: (MWC.Variate a, MonadIO m) => Producer m a
sourceRandom = sourceRandomWith MWC.uniform
{-# INLINE sourceRandom #-}

-- | Create a stream of random values of length n, seeding from the system
-- random number.
--
-- Subject to fusion
--
-- Since 1.0.0
sourceRandomN :: (MWC.Variate a, MonadIO m)
              => Int -- ^ count
              -> Producer m a
sourceRandomN cnt = sourceRandomNWith cnt MWC.uniform
{-# INLINE sourceRandomN #-}

-- | Create an infinite stream of random values, using the given random number
-- generator.
--
-- Subject to fusion
--
-- Since 1.0.0
sourceRandomGen :: (MWC.Variate a, MonadBase base m, PrimMonad base)
                => MWC.Gen (PrimState base)
                -> Producer m a
sourceRandomGen gen = sourceRandomGenWith gen MWC.uniform
{-# INLINE sourceRandomGen #-}

-- | Create a stream of random values of length n, seeding from the system
-- random number.
--
-- Subject to fusion
--
-- Since 1.0.0
sourceRandomNGen :: (MWC.Variate a, MonadBase base m, PrimMonad base)
                 => MWC.Gen (PrimState base)
                 -> Int -- ^ count
                 -> Producer m a
sourceRandomNGen gen cnt = sourceRandomNGenWith gen cnt MWC.uniform
{-# INLINE sourceRandomNGen #-}

-- | Create an infinite stream of random values from an arbitrary distribution,
-- seeding from the system random number.
--
-- Subject to fusion
--
-- Since 1.0.3
sourceRandomWith :: (MWC.Variate a, MonadIO m) => (MWC.GenIO -> SIO.IO a) -> Producer m a
INLINE_RULE(sourceRandomWith, f, initRepeat (liftIO MWC.createSystemRandom) (liftIO . f))

-- | Create a stream of random values of length n from an arbitrary
-- distribution, seeding from the system random number.
--
-- Subject to fusion
--
-- Since 1.0.3
sourceRandomNWith :: (MWC.Variate a, MonadIO m)
                  => Int -- ^ count
                  -> (MWC.GenIO -> SIO.IO a)
                  -> Producer m a
INLINE_RULE(sourceRandomNWith, cnt f, initReplicate (liftIO MWC.createSystemRandom) (liftIO . f) cnt)

-- | Create an infinite stream of random values from an arbitrary distribution,
-- using the given random number generator.
--
-- Subject to fusion
--
-- Since 1.0.3
sourceRandomGenWith :: (MWC.Variate a, MonadBase base m, PrimMonad base)
                    => MWC.Gen (PrimState base)
                    -> (MWC.Gen (PrimState base) -> base a)
                    -> Producer m a
INLINE_RULE(sourceRandomGenWith, gen f, initRepeat (return gen) (liftBase . f))

-- | Create a stream of random values of length n from an arbitrary
-- distribution, seeding from the system random number.
--
-- Subject to fusion
--
-- Since 1.0.3
sourceRandomNGenWith :: (MWC.Variate a, MonadBase base m, PrimMonad base)
                     => MWC.Gen (PrimState base)
                     -> Int -- ^ count
                     -> (MWC.Gen (PrimState base) -> base a)
                     -> Producer m a
INLINE_RULE(sourceRandomNGenWith, gen cnt f, initReplicate (return gen) (liftBase . f) cnt)

-- | Stream the contents of the given directory, without traversing deeply.
--
-- This function will return /all/ of the contents of the directory, whether
-- they be files, directories, etc.
--
-- Note that the generated filepaths will be the complete path, not just the
-- filename. In other words, if you have a directory @foo@ containing files
-- @bar@ and @baz@, and you use @sourceDirectory@ on @foo@, the results will be
-- @foo/bar@ and @foo/baz@.
--
-- Since 1.0.0
sourceDirectory :: MonadResource m => FilePath -> Producer m FilePath
sourceDirectory = CF.sourceDirectory

-- | Deeply stream the contents of the given directory.
--
-- This works the same as @sourceDirectory@, but will not return directories at
-- all. This function also takes an extra parameter to indicate whether
-- symlinks will be followed.
--
-- Since 1.0.0
sourceDirectoryDeep :: MonadResource m
                    => Bool -- ^ Follow directory symlinks
                    -> FilePath -- ^ Root directory
                    -> Producer m FilePath
sourceDirectoryDeep = CF.sourceDirectoryDeep

-- | Ignore a certain number of values in the stream.
--
-- Since 1.0.0
drop :: Monad m
     => Int
     -> Consumer a m ()
INLINE_RULE(drop, n, CL.drop n)

-- | Drop a certain number of elements from a chunked stream.
--
-- Since 1.0.0
dropE :: (Monad m, Seq.IsSequence seq)
      => Seq.Index seq
      -> Consumer seq m ()
dropE =
    loop
  where
    loop i = if i <= 0
        then return ()
        else await >>= maybe (return ()) (go i)

    go i sq = do
        unless (onull y) $ leftover y
        loop i'
      where
        (x, y) = Seq.splitAt i sq
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
    go x = if f x then loop else leftover x
{-# INLINE dropWhile #-}

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

    go sq =
        if onull x then loop else leftover x
      where
        x = Seq.dropWhile f sq
{-# INLINE dropWhileE #-}

-- | Monoidally combine all values in the stream.
--
-- Subject to fusion
--
-- Since 1.0.0
fold :: (Monad m, Monoid a)
     => Consumer a m a
INLINE_RULE0(fold, CL.foldMap id)

-- | Monoidally combine all elements in the chunked stream.
--
-- Subject to fusion
--
-- Since 1.0.0
foldE :: (Monad m, MonoFoldable mono, Monoid (Element mono))
      => Consumer mono m (Element mono)
INLINE_RULE0(foldE, CL.fold (\accum mono -> accum `mappend` ofoldMap id mono) mempty)

-- | A strict left fold.
--
-- Subject to fusion
--
-- Since 1.0.0
foldl :: Monad m => (a -> b -> a) -> a -> Consumer b m a
INLINE_RULE(foldl, f x, CL.fold f x)

-- | A strict left fold on a chunked stream.
--
-- Subject to fusion
--
-- Since 1.0.0
foldlE :: (Monad m, MonoFoldable mono)
       => (a -> Element mono -> a)
       -> a
       -> Consumer mono m a
INLINE_RULE(foldlE, f x, CL.fold (ofoldlPrime f) x)

-- Work around CPP not supporting identifiers with primes...
ofoldlPrime :: MonoFoldable mono => (a -> Element mono -> a) -> a -> mono -> a
ofoldlPrime = ofoldl'

-- | Apply the provided mapping function and monoidal combine all values.
--
-- Subject to fusion
--
-- Since 1.0.0
foldMap :: (Monad m, Monoid b)
        => (a -> b)
        -> Consumer a m b
INLINE_RULE(foldMap, f, CL.foldMap f)

-- | Apply the provided mapping function and monoidal combine all elements of the chunked stream.
--
-- Subject to fusion
--
-- Since 1.0.0
foldMapE :: (Monad m, MonoFoldable mono, Monoid w)
         => (Element mono -> w)
         -> Consumer mono m w
INLINE_RULE(foldMapE, f, CL.foldMap (ofoldMap f))

-- | A strict left fold with no starting value.  Returns 'Nothing'
-- when the stream is empty.
--
-- Subject to fusion
foldl1, foldl1C :: Monad m => (a -> a -> a) -> Consumer a m (Maybe a)
foldl1C f =
    await >>= maybe (return Nothing) loop
  where
    loop prev = await >>= maybe (return $ Just prev) (loop . f prev)
STREAMING(foldl1, foldl1C, foldl1S, f)

-- | A strict left fold on a chunked stream, with no starting value.
-- Returns 'Nothing' when the stream is empty.
--
-- Subject to fusion
--
-- Since 1.0.0
foldl1E :: (Monad m, MonoFoldable mono, a ~ Element mono)
        => (a -> a -> a)
        -> Consumer mono m (Maybe a)
INLINE_RULE(foldl1E, f, foldl (foldMaybeNull f) Nothing)

-- Helper for foldl1E
foldMaybeNull :: (MonoFoldable mono, e ~ Element mono)
              => (e -> e -> e)
              -> Maybe e
              -> mono
              -> Maybe e
foldMaybeNull f macc mono =
    case (macc, NonNull.fromNullable mono) of
        (Just acc, Just nn) -> Just $ ofoldl' f acc nn
        (Nothing, Just nn) -> Just $ NonNull.ofoldl1' f nn
        _ -> macc
{-# INLINE foldMaybeNull #-}

-- | Check that all values in the stream return True.
--
-- Subject to shortcut logic: at the first False, consumption of the stream
-- will stop.
--
-- Subject to fusion
--
-- Since 1.0.0
all, allC :: Monad m
          => (a -> Bool)
          -> Consumer a m Bool
allC f = fmap isNothing $ find (Prelude.not . f)
{-# INLINE allC #-}
STREAMING(all, allC, allS, f)

-- | Check that all elements in the chunked stream return True.
--
-- Subject to shortcut logic: at the first False, consumption of the stream
-- will stop.
--
-- Subject to fusion
--
-- Since 1.0.0
allE :: (Monad m, MonoFoldable mono)
     => (Element mono -> Bool)
     -> Consumer mono m Bool
INLINE_RULE(allE, f, all (oall f))

-- | Check that at least one value in the stream returns True.
--
-- Subject to shortcut logic: at the first True, consumption of the stream
-- will stop.
--
-- Subject to fusion
--
-- Since 1.0.0
any, anyC :: Monad m
          => (a -> Bool)
          -> Consumer a m Bool
anyC = fmap isJust . find
{-# INLINE anyC #-}
STREAMING(any, anyC, anyS, f)

-- | Check that at least one element in the chunked stream returns True.
--
-- Subject to shortcut logic: at the first True, consumption of the stream
-- will stop.
--
-- Subject to fusion
--
-- Since 1.0.0
anyE :: (Monad m, MonoFoldable mono)
     => (Element mono -> Bool)
     -> Consumer mono m Bool
INLINE_RULE(anyE, f, any (oany f))

-- | Are all values in the stream True?
--
-- Consumption stops once the first False is encountered.
--
-- Subject to fusion
--
-- Since 1.0.0
and :: Monad m => Consumer Bool m Bool
INLINE_RULE0(and, all id)

-- | Are all elements in the chunked stream True?
--
-- Consumption stops once the first False is encountered.
--
-- Subject to fusion
--
-- Since 1.0.0
andE :: (Monad m, MonoFoldable mono, Element mono ~ Bool)
     => Consumer mono m Bool
#if __GLASGOW_HASKELL__ >= 706
INLINE_RULE0(andE, allE id)
#else
andE = allE id
{-# INLINE andE #-}
#endif

-- | Are any values in the stream True?
--
-- Consumption stops once the first True is encountered.
--
-- Subject to fusion
--
-- Since 1.0.0
or :: Monad m => Consumer Bool m Bool
INLINE_RULE0(or, any id)

-- | Are any elements in the chunked stream True?
--
-- Consumption stops once the first True is encountered.
--
-- Subject to fusion
--
-- Since 1.0.0
orE :: (Monad m, MonoFoldable mono, Element mono ~ Bool)
    => Consumer mono m Bool
#if __GLASGOW_HASKELL__ >= 706
INLINE_RULE0(orE, anyE id)
#else
orE = anyE id
{-# INLINE orE #-}
#endif

-- | Are any values in the stream equal to the given value?
--
-- Stops consuming as soon as a match is found.
--
-- Subject to fusion
--
-- Since 1.0.0
elem :: (Monad m, Eq a) => a -> Consumer a m Bool
INLINE_RULE(elem, x, any (== x))

-- | Are any elements in the chunked stream equal to the given element?
--
-- Stops consuming as soon as a match is found.
--
-- Subject to fusion
--
-- Since 1.0.0
#if MIN_VERSION_mono_traversable(1,0,0)
elemE :: (Monad m, Seq.IsSequence seq, Eq (Element seq))
#else
elemE :: (Monad m, Seq.EqSequence seq)
#endif
      => Element seq
      -> Consumer seq m Bool
#if MIN_VERSION_mono_traversable(0,8,0)
INLINE_RULE(elemE, f, any (oelem f))
#else
INLINE_RULE(elemE, f, any (Seq.elem f))
#endif

-- | Are no values in the stream equal to the given value?
--
-- Stops consuming as soon as a match is found.
--
-- Subject to fusion
--
-- Since 1.0.0
notElem :: (Monad m, Eq a) => a -> Consumer a m Bool
INLINE_RULE(notElem, x, all (/= x))

-- | Are no elements in the chunked stream equal to the given element?
--
-- Stops consuming as soon as a match is found.
--
-- Subject to fusion
--
-- Since 1.0.0
#if MIN_VERSION_mono_traversable(1,0,0)
notElemE :: (Monad m, Seq.IsSequence seq, Eq (Element seq))
#else
notElemE :: (Monad m, Seq.EqSequence seq)
#endif
         => Element seq
         -> Consumer seq m Bool
#if MIN_VERSION_mono_traversable(0,8,0)
INLINE_RULE(notElemE, x, all (onotElem x))
#else
INLINE_RULE(notElemE, x, all (Seq.notElem x))
#endif

-- | Consume all incoming strict chunks into a lazy sequence.
-- Note that the entirety of the sequence will be resident at memory.
--
-- This can be used to consume a stream of strict ByteStrings into a lazy
-- ByteString, for example.
--
-- Subject to fusion
--
-- Since 1.0.0
sinkLazy, sinkLazyC :: (Monad m, LazySequence lazy strict)
                    => Consumer strict m lazy
sinkLazyC = (fromChunks . ($ [])) <$> CL.fold (\front next -> front . (next:)) id
{-# INLINE sinkLazyC #-}
STREAMING0(sinkLazy, sinkLazyC, sinkLazyS)

-- | Consume all values from the stream and return as a list. Note that this
-- will pull all values into memory.
--
-- Subject to fusion
--
-- Since 1.0.0
sinkList :: Monad m => Consumer a m [a]
INLINE_RULE0(sinkList, CL.consume)

-- | Sink incoming values into a vector, growing the vector as necessary to fit
-- more elements.
--
-- Note that using this function is more memory efficient than @sinkList@ and
-- then converting to a @Vector@, as it avoids intermediate list constructors.
--
-- Subject to fusion
--
-- Since 1.0.0
sinkVector, sinkVectorC :: (MonadBase base m, V.Vector v a, PrimMonad base)
                        => Consumer a m (v a)
sinkVectorC = do
    let initSize = 10
    mv0 <- liftBase $ VM.new initSize
    let go maxSize i mv | i >= maxSize = do
            let newMax = maxSize * 2
            mv' <- liftBase $ VM.grow mv maxSize
            go newMax i mv'
        go maxSize i mv = do
            mx <- await
            case mx of
                Nothing -> V.slice 0 i <$> liftBase (V.unsafeFreeze mv)
                Just x -> do
                    liftBase $ VM.write mv i x
                    go maxSize (i + 1) mv
    go initSize 0 mv0
{-# INLINEABLE sinkVectorC #-}
STREAMING0(sinkVector, sinkVectorC, sinkVectorS)

-- | Sink incoming values into a vector, up until size @maxSize@.  Subsequent
-- values will be left in the stream. If there are less than @maxSize@ values
-- present, returns a @Vector@ of smaller size.
--
-- Note that using this function is more memory efficient than @sinkList@ and
-- then converting to a @Vector@, as it avoids intermediate list constructors.
--
-- Subject to fusion
--
-- Since 1.0.0
sinkVectorN, sinkVectorNC :: (MonadBase base m, V.Vector v a, PrimMonad base)
                          => Int -- ^ maximum allowed size
                          -> Consumer a m (v a)
sinkVectorNC maxSize = do
    mv <- liftBase $ VM.new maxSize
    let go i | i >= maxSize = liftBase $ V.unsafeFreeze mv
        go i = do
            mx <- await
            case mx of
                Nothing -> V.slice 0 i <$> liftBase (V.unsafeFreeze mv)
                Just x -> do
                    liftBase $ VM.write mv i x
                    go (i + 1)
    go 0
{-# INLINEABLE sinkVectorNC #-}
STREAMING(sinkVectorN, sinkVectorNC, sinkVectorNS, maxSize)

-- | Convert incoming values to a builder and fold together all builder values.
--
-- Defined as: @foldMap toBuilder@.
--
-- Subject to fusion
--
-- Since 1.0.0
sinkBuilder :: (Monad m, Monoid builder, ToBuilder a builder)
            => Consumer a m builder
INLINE_RULE0(sinkBuilder, foldMap toBuilder)

-- | Same as @sinkBuilder@, but afterwards convert the builder to its lazy
-- representation.
--
-- Alternatively, this could be considered an alternative to @sinkLazy@, with
-- the following differences:
--
-- * This function will allow multiple input types, not just the strict version
-- of the lazy structure.
--
-- * Some buffer copying may occur in this version.
--
-- Subject to fusion
--
-- Since 1.0.0
sinkLazyBuilder, sinkLazyBuilderC :: (Monad m, Monoid builder, ToBuilder a builder, Builder builder lazy)
                                  => Consumer a m lazy
sinkLazyBuilderC = fmap builderToLazy sinkBuilder
{-# INLINE sinkLazyBuilderC #-}
STREAMING0(sinkLazyBuilder, sinkLazyBuilderC, sinkLazyBuilderS)

-- | Consume and discard all remaining values in the stream.
--
-- Subject to fusion
--
-- Since 1.0.0
sinkNull :: Monad m => Consumer a m ()
INLINE_RULE0(sinkNull, CL.sinkNull)

-- | Same as @await@, but discards any leading 'onull' values.
--
-- Since 1.0.0
awaitNonNull :: (Monad m, MonoFoldable a) => Consumer a m (Maybe (NonNull.NonNull a))
awaitNonNull =
    go
  where
    go = await >>= maybe (return Nothing) go'

    go' = maybe go (return . Just) . NonNull.fromNullable
{-# INLINE awaitNonNull #-}

-- | Get the next element in the chunked stream.
--
-- Since 1.0.0
headE :: (Monad m, Seq.IsSequence seq) => Consumer seq m (Maybe (Element seq))
headE =
    loop
  where
    loop = await >>= maybe (return Nothing) go
    go x =
        case Seq.uncons x of
            Nothing -> loop
            Just (y, z) -> do
                unless (onull z) $ leftover z
                return $ Just y
{-# INLINE headE #-}

-- | View the next value in the stream without consuming it.
--
-- Since 1.0.0
peek :: Monad m => Consumer a m (Maybe a)
peek = CL.peek
{-# INLINE peek #-}

-- | View the next element in the chunked stream without consuming it.
--
-- Since 1.0.0
peekE :: (Monad m, MonoFoldable mono) => Consumer mono m (Maybe (Element mono))
peekE =
    loop
  where
    loop = await >>= maybe (return Nothing) go
    go x =
        case headMay x of
            Nothing -> loop
            Just y -> do
                leftover x
                return $ Just y
{-# INLINE peekE #-}

-- | Retrieve the last value in the stream, if present.
--
-- Subject to fusion
--
-- Since 1.0.0
last, lastC :: Monad m => Consumer a m (Maybe a)
lastC =
    await >>= maybe (return Nothing) loop
  where
    loop prev = await >>= maybe (return $ Just prev) loop
STREAMING0(last, lastC, lastS)

-- | Retrieve the last element in the chunked stream, if present.
--
-- Subject to fusion
--
-- Since 1.0.0
lastE, lastEC :: (Monad m, Seq.IsSequence seq) => Consumer seq m (Maybe (Element seq))
lastEC =
    awaitNonNull >>= maybe (return Nothing) (loop . NonNull.last)
  where
    loop prev = awaitNonNull >>= maybe (return $ Just prev) (loop . NonNull.last)
STREAMING0(lastE, lastEC, lastES)

-- | Count how many values are in the stream.
--
-- Subject to fusion
--
-- Since 1.0.0
length :: (Monad m, Num len) => Consumer a m len
INLINE_RULE0(length, foldl (\x _ -> x + 1) 0)

-- | Count how many elements are in the chunked stream.
--
-- Subject to fusion
--
-- Since 1.0.0
lengthE :: (Monad m, Num len, MonoFoldable mono) => Consumer mono m len
INLINE_RULE0(lengthE, foldl (\x y -> x + fromIntegral (olength y)) 0)

-- | Count how many values in the stream pass the given predicate.
--
-- Subject to fusion
--
-- Since 1.0.0
lengthIf :: (Monad m, Num len) => (a -> Bool) -> Consumer a m len
INLINE_RULE(lengthIf, f, foldl (\cnt a -> if f a then (cnt + 1) else cnt) 0)

-- | Count how many elements in the chunked stream pass the given predicate.
--
-- Subject to fusion
--
-- Since 1.0.0
lengthIfE :: (Monad m, Num len, MonoFoldable mono)
          => (Element mono -> Bool) -> Consumer mono m len
INLINE_RULE(lengthIfE, f, foldlE (\cnt a -> if f a then (cnt + 1) else cnt) 0)

-- | Get the largest value in the stream, if present.
--
-- Subject to fusion
--
-- Since 1.0.0
maximum :: (Monad m, Ord a) => Consumer a m (Maybe a)
INLINE_RULE0(maximum, foldl1 max)

-- | Get the largest element in the chunked stream, if present.
--
-- Subject to fusion
--
-- Since 1.0.0
#if MIN_VERSION_mono_traversable(1,0,0)
maximumE :: (Monad m, Seq.IsSequence seq, Ord (Element seq)) => Consumer seq m (Maybe (Element seq))
#else
maximumE :: (Monad m, Seq.OrdSequence seq) => Consumer seq m (Maybe (Element seq))
#endif
INLINE_RULE0(maximumE, foldl1E max)

-- | Get the smallest value in the stream, if present.
--
-- Subject to fusion
--
-- Since 1.0.0
minimum :: (Monad m, Ord a) => Consumer a m (Maybe a)
INLINE_RULE0(minimum, foldl1 min)

-- | Get the smallest element in the chunked stream, if present.
--
-- Subject to fusion
--
-- Since 1.0.0
#if MIN_VERSION_mono_traversable(1,0,0)
minimumE :: (Monad m, Seq.IsSequence seq, Ord (Element seq)) => Consumer seq m (Maybe (Element seq))
#else
minimumE :: (Monad m, Seq.OrdSequence seq) => Consumer seq m (Maybe (Element seq))
#endif
INLINE_RULE0(minimumE, foldl1E min)

-- | True if there are no values in the stream.
--
-- This function does not modify the stream.
--
-- Since 1.0.0
null :: Monad m => Consumer a m Bool
null = (maybe True (\_ -> False)) `fmap` peek
{-# INLINE null #-}

-- | True if there are no elements in the chunked stream.
--
-- This function may remove empty leading chunks from the stream, but otherwise
-- will not modify it.
--
-- Since 1.0.0
nullE :: (Monad m, MonoFoldable mono)
      => Consumer mono m Bool
nullE =
    go
  where
    go = await >>= maybe (return True) go'
    go' x = if onull x then go else leftover x >> return False
{-# INLINE nullE #-}

-- | Get the sum of all values in the stream.
--
-- Subject to fusion
--
-- Since 1.0.0
sum :: (Monad m, Num a) => Consumer a m a
INLINE_RULE0(sum, foldl (+) 0)

-- | Get the sum of all elements in the chunked stream.
--
-- Subject to fusion
--
-- Since 1.0.0
sumE :: (Monad m, MonoFoldable mono, Num (Element mono)) => Consumer mono m (Element mono)
INLINE_RULE0(sumE, foldlE (+) 0)

-- | Get the product of all values in the stream.
--
-- Subject to fusion
--
-- Since 1.0.0
product :: (Monad m, Num a) => Consumer a m a
INLINE_RULE0(product, foldl (*) 1)

-- | Get the product of all elements in the chunked stream.
--
-- Subject to fusion
--
-- Since 1.0.0
productE :: (Monad m, MonoFoldable mono, Num (Element mono)) => Consumer mono m (Element mono)
INLINE_RULE0(productE, foldlE (*) 1)

-- | Find the first matching value.
--
-- Subject to fusion
--
-- Since 1.0.0
find, findC :: Monad m => (a -> Bool) -> Consumer a m (Maybe a)
findC f =
    loop
  where
    loop = await >>= maybe (return Nothing) go
    go x = if f x then return (Just x) else loop
{-# INLINE findC #-}
STREAMING(find, findC, findS, f)

-- | Apply the action to all values in the stream.
--
-- Subject to fusion
--
-- Since 1.0.0
mapM_ :: Monad m => (a -> m ()) -> Consumer a m ()
INLINE_RULE(mapM_, f, CL.mapM_ f)

-- | Apply the action to all elements in the chunked stream.
--
-- Subject to fusion
--
-- Since 1.0.0
mapM_E :: (Monad m, MonoFoldable mono) => (Element mono -> m ()) -> Consumer mono m ()
INLINE_RULE(mapM_E, f, CL.mapM_ (omapM_ f))

-- | A monadic strict left fold.
--
-- Subject to fusion
--
-- Since 1.0.0
foldM :: Monad m => (a -> b -> m a) -> a -> Consumer b m a
INLINE_RULE(foldM, f x, CL.foldM f x)

-- | A monadic strict left fold on a chunked stream.
--
-- Subject to fusion
--
-- Since 1.0.0
foldME :: (Monad m, MonoFoldable mono)
       => (a -> Element mono -> m a)
       -> a
       -> Consumer mono m a
INLINE_RULE(foldME, f x, foldM (ofoldlM f) x)

-- | Apply the provided monadic mapping function and monoidal combine all values.
--
-- Subject to fusion
--
-- Since 1.0.0
foldMapM :: (Monad m, Monoid w) => (a -> m w) -> Consumer a m w
INLINE_RULE(foldMapM, f, CL.foldMapM f)

-- | Apply the provided monadic mapping function and monoidal combine all
-- elements in the chunked stream.
--
-- Subject to fusion
--
-- Since 1.0.0
foldMapME :: (Monad m, MonoFoldable mono, Monoid w)
          => (Element mono -> m w)
          -> Consumer mono m w
INLINE_RULE(foldMapME, f, CL.foldM (ofoldlM (\accum e -> mappend accum `liftM` f e)) mempty)

-- | Write all data to the given file.
--
-- This function automatically opens and closes the file handle, and ensures
-- exception safety via @MonadResource@. It works for all instances of @IOData@,
-- including @ByteString@ and @Text@.
--
-- Since 1.0.0
sinkFile :: (MonadResource m, IOData a) => FilePath -> Consumer a m ()
sinkFile fp = sinkIOHandle (SIO.openFile fp SIO.WriteMode)
{-# INLINE sinkFile #-}

-- | Print all incoming values to stdout.
--
-- Subject to fusion
--
-- Since 1.0.0
print :: (Show a, MonadIO m) => Consumer a m ()
INLINE_RULE0(print, mapM_ (liftIO . Prelude.print))

-- | @sinkHandle@ applied to @stdout@.
--
-- Subject to fusion
--
-- Since 1.0.0
stdout :: (MonadIO m, IOData a) => Consumer a m ()
INLINE_RULE0(stdout, sinkHandle SIO.stdout)

-- | @sinkHandle@ applied to @stderr@.
--
-- Subject to fusion
--
-- Since 1.0.0
stderr :: (MonadIO m, IOData a) => Consumer a m ()
INLINE_RULE0(stderr, sinkHandle SIO.stderr)

-- | Write all data to the given @Handle@.
--
-- Does not close the @Handle@ at any point.
--
-- Subject to fusion
--
-- Since 1.0.0
sinkHandle :: (MonadIO m, IOData a) => Handle -> Consumer a m ()
INLINE_RULE(sinkHandle, h, CL.mapM_ (hPut h))

-- | Open a @Handle@ using the given function and stream data to it.
--
-- Automatically closes the file at completion.
--
-- Since 1.0.0
sinkIOHandle :: (MonadResource m, IOData a) => SIO.IO Handle -> Consumer a m ()
sinkIOHandle alloc = bracketP alloc SIO.hClose sinkHandle
{-# INLINE sinkIOHandle #-}

-- | Apply a transformation to all values in a stream.
--
-- Subject to fusion
--
-- Since 1.0.0
map :: Monad m => (a -> b) -> Conduit a m b
INLINE_RULE(map, f, CL.map f)

-- | Apply a transformation to all elements in a chunked stream.
--
-- Subject to fusion
--
-- Since 1.0.0
mapE :: (Monad m, Functor f) => (a -> b) -> Conduit (f a) m (f b)
INLINE_RULE(mapE, f, CL.map (fmap f))

-- | Apply a monomorphic transformation to all elements in a chunked stream.
--
-- Unlike @mapE@, this will work on types like @ByteString@ and @Text@ which
-- are @MonoFunctor@ but not @Functor@.
--
-- Subject to fusion
--
-- Since 1.0.0
omapE :: (Monad m, MonoFunctor mono) => (Element mono -> Element mono) -> Conduit mono m mono
INLINE_RULE(omapE, f, CL.map (omap f))

-- | Apply the function to each value in the stream, resulting in a foldable
-- value (e.g., a list). Then yield each of the individual values in that
-- foldable value separately.
--
-- Generalizes concatMap, mapMaybe, and mapFoldable.
--
-- Subject to fusion
--
-- Since 1.0.0
concatMap, concatMapC :: (Monad m, MonoFoldable mono)
                      => (a -> mono)
                      -> Conduit a m (Element mono)
concatMapC f = awaitForever (yieldMany . f)
{-# INLINE concatMapC #-}
STREAMING(concatMap, concatMapC, concatMapS, f)

-- | Apply the function to each element in the chunked stream, resulting in a
-- foldable value (e.g., a list). Then yield each of the individual values in
-- that foldable value separately.
--
-- Generalizes concatMap, mapMaybe, and mapFoldable.
--
-- Subject to fusion
--
-- Since 1.0.0
concatMapE :: (Monad m, MonoFoldable mono, Monoid w)
           => (Element mono -> w)
           -> Conduit mono m w
INLINE_RULE(concatMapE, f, CL.map (ofoldMap f))

-- | Stream up to n number of values downstream.
--
-- Note that, if downstream terminates early, not all values will be consumed.
-- If you want to force /exactly/ the given number of values to be consumed,
-- see 'takeExactly'.
--
-- Subject to fusion
--
-- Since 1.0.0
take :: Monad m => Int -> Conduit a m a
INLINE_RULE(take, n, CL.isolate n)

-- | Stream up to n number of elements downstream in a chunked stream.
--
-- Note that, if downstream terminates early, not all values will be consumed.
-- If you want to force /exactly/ the given number of values to be consumed,
-- see 'takeExactlyE'.
--
-- Since 1.0.0
takeE :: (Monad m, Seq.IsSequence seq)
      => Seq.Index seq
      -> Conduit seq m seq
takeE =
    loop
  where
    loop i = if i <= 0
        then return ()
        else await >>= maybe (return ()) (go i)

    go i sq = do
        unless (onull x) $ yield x
        unless (onull y) $ leftover y
        loop i'
      where
        (x, y) = Seq.splitAt i sq
        i' = i - fromIntegral (olength x)
{-# INLINEABLE takeE #-}

-- | Stream all values downstream that match the given predicate.
--
-- Same caveats regarding downstream termination apply as with 'take'.
--
-- Since 1.0.0
takeWhile :: Monad m
          => (a -> Bool)
          -> Conduit a m a
takeWhile f =
    loop
  where
    loop = await >>= maybe (return ()) go
    go x = if f x
        then yield x >> loop
        else leftover x
{-# INLINE takeWhile #-}

-- | Stream all elements downstream that match the given predicate in a chunked stream.
--
-- Same caveats regarding downstream termination apply as with 'takeE'.
--
-- Since 1.0.0
takeWhileE :: (Monad m, Seq.IsSequence seq)
           => (Element seq -> Bool)
           -> Conduit seq m seq
takeWhileE f =
    loop
  where
    loop = await >>= maybe (return ()) go

    go sq = do
        unless (onull x) $ yield x
        if onull y
            then loop
            else leftover y
      where
        (x, y) = Seq.span f sq
{-# INLINE takeWhileE #-}

-- | Consume precisely the given number of values and feed them downstream.
--
-- This function is in contrast to 'take', which will only consume up to the
-- given number of values, and will terminate early if downstream terminates
-- early. This function will discard any additional values in the stream if
-- they are unconsumed.
--
-- Note that this function takes a downstream @ConduitM@ as a parameter, as
-- opposed to working with normal fusion. For more information, see
-- <http://www.yesodweb.com/blog/2013/10/core-flaw-pipes-conduit>, the section
-- titled \"pipes and conduit: isolate\".
--
-- Since 1.0.0
takeExactly :: Monad m
            => Int
            -> ConduitM a b m r
            -> ConduitM a b m r
takeExactly count inner = take count =$= do
    r <- inner
    CL.sinkNull
    return r

-- | Same as 'takeExactly', but for chunked streams.
--
-- Since 1.0.0
takeExactlyE :: (Monad m, Seq.IsSequence a)
             => Seq.Index a
             -> ConduitM a b m r
             -> ConduitM a b m r
takeExactlyE count inner = takeE count =$= do
    r <- inner
    CL.sinkNull
    return r
{-# INLINE takeExactlyE #-}

-- | Flatten out a stream by yielding the values contained in an incoming
-- @MonoFoldable@ as individually yielded values.
--
-- Subject to fusion
--
-- Since 1.0.0
concat, concatC :: (Monad m, MonoFoldable mono)
                => Conduit mono m (Element mono)
concatC = awaitForever yieldMany
STREAMING0(concat, concatC, concatS)

-- | Keep only values in the stream passing a given predicate.
--
-- Subject to fusion
--
-- Since 1.0.0
filter :: Monad m => (a -> Bool) -> Conduit a m a
INLINE_RULE(filter, f, CL.filter f)

-- | Keep only elements in the chunked stream passing a given predicate.
--
-- Subject to fusion
--
-- Since 1.0.0
filterE :: (Seq.IsSequence seq, Monad m) => (Element seq -> Bool) -> Conduit seq m seq
INLINE_RULE(filterE, f, CL.map (Seq.filter f))

-- | Map values as long as the result is @Just@.
--
-- Since 1.0.0
mapWhile :: Monad m => (a -> Maybe b) -> Conduit a m b
mapWhile f =
    loop
  where
    loop = await >>= maybe (return ()) go
    go x =
        case f x of
            Just y -> yield y >> loop
            Nothing -> leftover x
{-# INLINE mapWhile #-}

-- | Break up a stream of values into vectors of size n. The final vector may
-- be smaller than n if the total number of values is not a strict multiple of
-- n. No empty vectors will be yielded.
--
-- Since 1.0.0
conduitVector :: (MonadBase base m, V.Vector v a, PrimMonad base)
              => Int -- ^ maximum allowed size
              -> Conduit a m (v a)
conduitVector size =
    loop
  where
    loop = do
        v <- sinkVectorN size
        unless (V.null v) $ do
            yield v
            loop
{-# INLINE conduitVector #-}

-- | Analog of 'Prelude.scanl' for lists.
--
-- Subject to fusion
--
-- Since 1.0.6
scanl, scanlC :: Monad m => (a -> b -> a) -> a -> Conduit b m a
scanlC f =
    loop
  where
    loop seed =
        await >>= maybe (yield seed) go
      where
        go b = do
            let seed' = f seed b
            seed' `seq` yield seed
            loop seed'
STREAMING(scanl, scanlC, scanlS, f x)

-- | 'mapWhile' with a break condition dependent on a strict accumulator.
-- Equivalently, 'CL.mapAccum' as long as the result is @Right@. Instead of
-- producing a leftover, the breaking input determines the resulting
-- accumulator via @Left@.
--
-- Subject to fusion
mapAccumWhile, mapAccumWhileC :: Monad m =>
    (a -> s -> Either s (s, b)) -> s -> ConduitM a b m s
mapAccumWhileC f =
    loop
  where
    loop !s = await >>= maybe (return s) go
      where
        go a = either (return $!) (\(s', b) -> yield b >> loop s') $ f a s
{-# INLINE mapAccumWhileC #-}
STREAMING(mapAccumWhile, mapAccumWhileC, mapAccumWhileS, f s)

-- | 'concatMap' with an accumulator.
--
-- Subject to fusion
--
-- Since 1.0.0
concatMapAccum :: Monad m => (a -> accum -> (accum, [b])) -> accum -> Conduit a m b
INLINE_RULE0(concatMapAccum, CL.concatMapAccum)

-- | Insert the given value between each two values in the stream.
--
-- Subject to fusion
--
-- Since 1.0.0
intersperse, intersperseC :: Monad m => a -> Conduit a m a
intersperseC x =
    await >>= omapM_ go
  where
    go y = yield y >> concatMap (\z -> [x, z])
STREAMING(intersperse, intersperseC, intersperseS, x)

-- | Sliding window of values
-- 1,2,3,4,5 with window size 2 gives
-- [1,2],[2,3],[3,4],[4,5]
--
-- Best used with structures that support O(1) snoc.
--
-- Subject to fusion
--
-- Since 1.0.0
slidingWindow, slidingWindowC :: (Monad m, Seq.IsSequence seq, Element seq ~ a) => Int -> Conduit a m seq
slidingWindowC sz = go (max 1 sz) mempty
    where goContinue st = await >>=
                          maybe (return ())
                                (\x -> do
                                   let st' = Seq.snoc st x
                                   yield st' >> goContinue (Seq.unsafeTail st')
                                )
          go 0 st = yield st >> goContinue (Seq.unsafeTail st)
          go !n st = CL.head >>= \m ->
                     case m of
                       Nothing -> yield st
                       Just x -> go (n-1) (Seq.snoc st x)
STREAMING(slidingWindow, slidingWindowC, slidingWindowS, sz)

codeWith :: Monad m
         => Int
         -> (ByteString -> Either e ByteString)
         -> Conduit ByteString m ByteString
codeWith size f =
    loop
  where
    loop = await >>= maybe (return ()) push

    loopWith bs
        | S.null bs = loop
        | otherwise = await >>= maybe (finish bs) (pushWith bs)

    finish bs =
        case f bs of
            Left _ -> leftover bs
            Right x -> yield x

    push bs = do
        let (x, y) = S.splitAt (len - (len `mod` size)) bs
        if S.null x
            then loopWith y
            else do
                case f x of
                    Left _ -> leftover bs
                    Right x' -> yield x' >> loopWith y
      where
        len = olength bs

    pushWith bs1 bs2 | S.length bs1 + S.length bs2 < size = loopWith (S.append bs1 bs2)
    pushWith bs1 bs2 = assertion1 $ assertion2 $ assertion3 $
        case f bs1' of
            Left _ -> leftover bs2 >> leftover bs1
            Right toYield -> yield toYield >> push y
      where
        m = S.length bs1 `mod` size
        (x, y) = S.splitAt (size - m) bs2
        bs1' = mappend bs1 x

        assertion1 = assert $ olength bs1 < size
        assertion2 = assert $ olength bs1' `mod` size == 0
        assertion3 = assert $ olength bs1' > 0

-- | Apply base64-encoding to the stream.
--
-- Since 1.0.0
encodeBase64 :: Monad m => Conduit ByteString m ByteString
encodeBase64 = codeWith 3 (Right . B64.encode)
{-# INLINE encodeBase64 #-}

-- | Apply base64-decoding to the stream. Will stop decoding on the first
-- invalid chunk.
--
-- Since 1.0.0
decodeBase64 :: Monad m => Conduit ByteString m ByteString
decodeBase64 = codeWith 4 B64.decode
{-# INLINE decodeBase64 #-}

-- | Apply URL-encoding to the stream.
--
-- Since 1.0.0
encodeBase64URL :: Monad m => Conduit ByteString m ByteString
encodeBase64URL = codeWith 3 (Right . B64U.encode)
{-# INLINE encodeBase64URL #-}

-- | Apply lenient base64URL-decoding to the stream. Will stop decoding on the
-- first invalid chunk.
--
-- Since 1.0.0
decodeBase64URL :: Monad m => Conduit ByteString m ByteString
decodeBase64URL = codeWith 4 B64U.decode
{-# INLINE decodeBase64URL #-}

-- | Apply base16-encoding to the stream.
--
-- Subject to fusion
--
-- Since 1.0.0
encodeBase16 :: Monad m => Conduit ByteString m ByteString
INLINE_RULE0(encodeBase16, map B16.encode)

-- | Apply base16-decoding to the stream. Will stop decoding on the first
-- invalid chunk.
--
-- Since 1.0.0
decodeBase16 :: Monad m => Conduit ByteString m ByteString
decodeBase16 =
    codeWith 2 decode'
  where
    decode' x
        | onull z = Right y
        | otherwise = Left ()
      where
        (y, z) = B16.decode x
{-# INLINE decodeBase16 #-}

-- | Apply a monadic transformation to all values in a stream.
--
-- If you do not need the transformed values, and instead just want the monadic
-- side-effects of running the action, see 'mapM_'.
--
-- Subject to fusion
--
-- Since 1.0.0
mapM :: Monad m => (a -> m b) -> Conduit a m b
INLINE_RULE(mapM, f, CL.mapM f)

-- | Apply a monadic transformation to all elements in a chunked stream.
--
-- Subject to fusion
--
-- Since 1.0.0
mapME :: (Monad m, Data.Traversable.Traversable f) => (a -> m b) -> Conduit (f a) m (f b)
INLINE_RULE(mapME, f, CL.mapM (Data.Traversable.mapM f))

-- | Apply a monadic monomorphic transformation to all elements in a chunked stream.
--
-- Unlike @mapME@, this will work on types like @ByteString@ and @Text@ which
-- are @MonoFunctor@ but not @Functor@.
--
-- Subject to fusion
--
-- Since 1.0.0
omapME :: (Monad m, MonoTraversable mono)
       => (Element mono -> m (Element mono))
       -> Conduit mono m mono
INLINE_RULE(omapME, f, CL.mapM (omapM f))

-- | Apply the monadic function to each value in the stream, resulting in a
-- foldable value (e.g., a list). Then yield each of the individual values in
-- that foldable value separately.
--
-- Generalizes concatMapM, mapMaybeM, and mapFoldableM.
--
-- Subject to fusion
--
-- Since 1.0.0
concatMapM, concatMapMC :: (Monad m, MonoFoldable mono)
                        => (a -> m mono)
                        -> Conduit a m (Element mono)
concatMapMC f = awaitForever (lift . f >=> yieldMany)
STREAMING(concatMapM, concatMapMC, concatMapMS, f)

-- | Keep only values in the stream passing a given monadic predicate.
--
-- Subject to fusion
--
-- Since 1.0.0
filterM, filterMC :: Monad m
                  => (a -> m Bool)
                  -> Conduit a m a
filterMC f =
    awaitForever go
  where
    go x = do
        b <- lift $ f x
        when b $ yield x
STREAMING(filterM, filterMC, filterMS, f)

-- | Keep only elements in the chunked stream passing a given monadic predicate.
--
-- Subject to fusion
--
-- Since 1.0.0
filterME :: (Monad m, Seq.IsSequence seq) => (Element seq -> m Bool) -> Conduit seq m seq
INLINE_RULE(filterME, f, CL.mapM (Seq.filterM f))

-- | Apply a monadic action on all values in a stream.
--
-- This @Conduit@ can be used to perform a monadic side-effect for every
-- value, whilst passing the value through the @Conduit@ as-is.
--
-- > iterM f = mapM (\a -> f a >>= \() -> return a)
--
-- Subject to fusion
--
-- Since 1.0.0
iterM :: Monad m => (a -> m ()) -> Conduit a m a
INLINE_RULE(iterM, f, CL.iterM f)

-- | Analog of 'Prelude.scanl' for lists, monadic.
--
-- Subject to fusion
--
-- Since 1.0.6
scanlM, scanlMC :: Monad m => (a -> b -> m a) -> a -> Conduit b m a
scanlMC f =
    loop
  where
    loop seed =
        await >>= maybe (yield seed) go
      where
        go b = do
            seed' <- lift $ f seed b
            seed' `seq` yield seed
            loop seed'
STREAMING(scanlM, scanlMC, scanlMS, f x)

-- | Monadic `mapAccumWhile`.
--
-- Subject to fusion
mapAccumWhileM, mapAccumWhileMC :: Monad m =>
    (a -> s -> m (Either s (s, b))) -> s -> ConduitM a b m s
mapAccumWhileMC f =
    loop
  where
    loop !s = await >>= maybe (return s) go
      where
        go a = lift (f a s) >>= either (return $!) (\(s', b) -> yield b >> loop s')
{-# INLINE mapAccumWhileMC #-}
STREAMING(mapAccumWhileM, mapAccumWhileMC, mapAccumWhileMS, f s)

-- | 'concatMapM' with an accumulator.
--
-- Subject to fusion
--
-- Since 1.0.0
concatMapAccumM :: Monad m => (a -> accum -> m (accum, [b])) -> accum -> Conduit a m b
INLINE_RULE(concatMapAccumM, f x, CL.concatMapAccumM f x)

-- | Encode a stream of text as UTF8.
--
-- Subject to fusion
--
-- Since 1.0.0
encodeUtf8 :: (Monad m, DTE.Utf8 text binary) => Conduit text m binary
INLINE_RULE0(encodeUtf8, map DTE.encodeUtf8)

-- | Decode a stream of binary data as UTF8.
--
-- Since 1.0.0
decodeUtf8 :: MonadThrow m => Conduit ByteString m Text
decodeUtf8 = CT.decode CT.utf8

-- | Decode a stream of binary data as UTF8, replacing any invalid bytes with
-- the Unicode replacement character.
--
-- Since 1.0.0
decodeUtf8Lenient :: MonadThrow m => Conduit ByteString m Text
decodeUtf8Lenient = CT.decodeUtf8Lenient

-- | Stream in the entirety of a single line.
--
-- Like @takeExactly@, this will consume the entirety of the line regardless of
-- the behavior of the inner Conduit.
--
-- Since 1.0.0
line :: (Monad m, Seq.IsSequence seq, Element seq ~ Char)
     => ConduitM seq o m r
     -> ConduitM seq o m r
line = takeExactlyUntilE (== '\n')
{-# INLINE line #-}

-- | Same as 'line', but operates on ASCII/binary data.
--
-- Since 1.0.0
lineAscii :: (Monad m, Seq.IsSequence seq, Element seq ~ Word8)
          => ConduitM seq o m r
          -> ConduitM seq o m r
lineAscii = takeExactlyUntilE (== 10)
{-# INLINE lineAscii #-}

-- | Stream in the chunked input until an element matches a predicate.
--
-- Like @takeExactly@, this will consume the entirety of the prefix
-- regardless of the behavior of the inner Conduit.
takeExactlyUntilE :: (Monad m, Seq.IsSequence seq)
                  => (Element seq -> Bool)
                  -> ConduitM seq o m r
                  -> ConduitM seq o m r
takeExactlyUntilE f inner =
    loop =$= do
        x <- inner
        sinkNull
        return x
  where
    loop = await >>= omapM_ go
    go t =
        if onull y
            then yield x >> loop
            else do
                unless (onull x) $ yield x
                let y' = Seq.drop 1 y
                unless (onull y') $ leftover y'
      where
        (x, y) = Seq.break f t
{-# INLINE takeExactlyUntilE #-}

-- | Insert a newline character after each incoming chunk of data.
--
-- Subject to fusion
--
-- Since 1.0.0
unlines :: (Monad m, Seq.IsSequence seq, Element seq ~ Char) => Conduit seq m seq
#if __GLASGOW_HASKELL__ >= 706
INLINE_RULE0(unlines, concatMap (:[Seq.singleton '\n']))
#else
unlines = concatMap (:[Seq.singleton '\n'])
{-# INLINE unlines #-}
#endif

-- | Same as 'unlines', but operates on ASCII/binary data.
--
-- Subject to fusion
--
-- Since 1.0.0
unlinesAscii :: (Monad m, Seq.IsSequence seq, Element seq ~ Word8) => Conduit seq m seq
#if __GLASGOW_HASKELL__ >= 706
INLINE_RULE0(unlinesAscii, concatMap (:[Seq.singleton 10]))
#else
unlinesAscii = concatMap (:[Seq.singleton 10])
#endif

-- | Split a stream of arbitrarily-chunked data, based on a predicate
-- on elements.  Elements that satisfy the predicate will cause chunks
-- to be split, and aren't included in these output chunks.  Note
-- that, if you have unknown or untrusted input, this function is
-- /unsafe/, since it would allow an attacker to form chunks of
-- massive length and exhaust memory.
splitOnUnboundedE, splitOnUnboundedEC
    :: (Monad m, Seq.IsSequence seq)
    => (Element seq -> Bool) -> Conduit seq m seq
splitOnUnboundedEC f =
    start
  where
    start = await >>= maybe (return ()) loop

    loop t =
        if onull y
            then do
                mt <- await
                case mt of
                    Nothing -> unless (onull t) $ yield t
                    Just t' -> loop (t `mappend` t')
            else yield x >> loop (Seq.drop 1 y)
      where
        (x, y) = Seq.break f t
STREAMING(splitOnUnboundedE, splitOnUnboundedEC, splitOnUnboundedES, f)

-- | Convert a stream of arbitrarily-chunked textual data into a stream of data
-- where each chunk represents a single line. Note that, if you have
-- unknown or untrusted input, this function is /unsafe/, since it would allow an
-- attacker to form lines of massive length and exhaust memory.
--
-- Subject to fusion
--
-- Since 1.0.0
linesUnbounded :: (Monad m, Seq.IsSequence seq, Element seq ~ Char)
               => Conduit seq m seq
#if __GLASGOW_HASKELL__ >= 706
INLINE_RULE0(linesUnbounded, splitOnUnboundedE (== '\n'))
#else
linesUnbounded = splitOnUnboundedE (== '\n')
#endif

-- | Same as 'linesUnbounded', but for ASCII/binary data.
--
-- Subject to fusion
--
-- Since 1.0.0
linesUnboundedAscii :: (Monad m, Seq.IsSequence seq, Element seq ~ Word8)
                    => Conduit seq m seq
#if __GLASGOW_HASKELL__ >= 706
INLINE_RULE0(linesUnboundedAscii, splitOnUnboundedE (== 10))
#else
linesUnboundedAscii = splitOnUnboundedE (== 10)
#endif

-- | Generally speaking, yielding values from inside a Conduit requires
-- some allocation for constructors. This can introduce an overhead,
-- similar to the overhead needed to represent a list of values instead of
-- a vector. This overhead is even more severe when talking about unboxed
-- values.
--
-- This combinator allows you to overcome this overhead, and efficiently
-- fill up vectors. It takes two parameters. The first is the size of each
-- mutable vector to be allocated. The second is a function. The function
-- takes an argument which will yield the next value into a mutable
-- vector.
--
-- Under the surface, this function uses a number of tricks to get high
-- performance. For more information on both usage and implementation,
-- please see:
-- <https://www.fpcomplete.com/user/snoyberg/library-documentation/vectorbuilder>
--
-- Since 1.0.0
vectorBuilder :: (PrimMonad base, MonadBase base m, V.Vector v e, MonadBase base n)
              => Int -- ^ size
              -> ((e -> n ()) -> Sink i m r)
              -> ConduitM i (v e) m r
vectorBuilder size inner = do
    ref <- liftBase $ do
        mv <- VM.new size
        newMutVar $! S 0 mv id
    res <- onAwait (yieldS ref) (inner (liftBase . addE ref))
    vs <- liftBase $ do
        S idx mv front <- readMutVar ref
        end <-
            if idx == 0
                then return []
                else do
                    v <- V.unsafeFreeze mv
                    return [V.unsafeTake idx v]
        return $ front end
    Prelude.mapM_ yield vs
    return res
{-# INLINE vectorBuilder #-}

data S s v e = S
    {-# UNPACK #-} !Int -- index
    !(V.Mutable v s e)
    ([v e] -> [v e])

onAwait :: Monad m
        => ConduitM i o m ()
        -> Sink i m r
        -> ConduitM i o m r
onAwait (ConduitM callback) (ConduitM sink0) = ConduitM $ \rest -> let
    go (Done r) = rest r
    go (HaveOutput _ _ o) = absurd o
    go (NeedInput f g) = callback $ \() -> NeedInput (go . f) (go . g)
    go (PipeM mp) = PipeM (liftM go mp)
    go (Leftover f i) = Leftover (go f) i
    in go (sink0 Done)
{-# INLINE onAwait #-}

yieldS :: (PrimMonad base, MonadBase base m)
       => MutVar (PrimState base) (S (PrimState base) v e)
       -> Producer m (v e)
yieldS ref = do
    S idx mv front <- liftBase $ readMutVar ref
    Prelude.mapM_ yield (front [])
    liftBase $ writeMutVar ref $! S idx mv id
{-# INLINE yieldS #-}

addE :: (PrimMonad m, V.Vector v e)
     => MutVar (PrimState m) (S (PrimState m) v e)
     -> e
     -> m ()
addE ref e = do
    S idx mv front <- readMutVar ref
    VM.write mv idx e
    let idx' = succ idx
        size = VM.length mv
    if idx' >= size
        then do
            v <- V.unsafeFreeze mv
            let front' = front . (v:)
            mv' <- VM.new size
            writeMutVar ref $! S 0 mv' front'
        else writeMutVar ref $! S idx' mv front
{-# INLINE addE #-}

-- | Consume a source with a strict accumulator, in a way piecewise defined by
-- a controlling stream. The latter will be evaluated until it terminates.
--
-- >>> let f a s = liftM (:s) $ mapC (*a) =$ CL.take a
-- >>> reverse $ runIdentity $ yieldMany [0..3] $$ mapAccumS f [] (yieldMany [1..])
-- [[],[1],[4,6],[12,15,18]] :: [[Int]]
mapAccumS :: Monad m => (a -> s -> Sink b m s) -> s -> Source m b -> Sink a m s
mapAccumS f s xs = do
    (zs, u) <- loop (newResumableSource xs, s)
    lift (closeResumableSource zs) >> return u
    where loop r@(ys, !t) = await >>= maybe (return r) go
              where go a  = lift (ys $$++ f a t) >>= loop
{-# INLINE mapAccumS #-}

-- | Run a consuming conduit repeatedly, only stopping when there is no more
-- data available from upstream.
--
-- Since 1.0.0
peekForever :: Monad m => ConduitM i o m () -> ConduitM i o m ()
peekForever inner =
    loop
  where
    loop = do
        mx <- peek
        case mx of
            Nothing -> return ()
            Just _ -> inner >> loop
