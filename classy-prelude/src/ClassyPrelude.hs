{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude
    ( -- * CorePrelude
      module CorePrelude
    , undefined
      -- * Standard
      -- ** Monoid
    , (++)
      -- ** Semigroup
    , Semigroup (..)
    , WrappedMonoid
      -- ** Functor
    , module Data.Functor
      -- ** Applicative
    , module Control.Applicative
    , (<&&>)
    , (<||>)
      -- ** Monad
    , module Control.Monad
    , whenM
    , unlessM
      -- ** UnliftIO reexports
    , module UnliftIO
      -- ** Mutable references
    , orElseSTM
    , module Data.Mutable
      -- ** STM Channels
    , module Control.Concurrent.STM.TBChan
    , module Control.Concurrent.STM.TBMChan
    , module Control.Concurrent.STM.TBMQueue
    , module Control.Concurrent.STM.TMChan
    , module Control.Concurrent.STM.TMQueue
      -- ** Primitive (exported since 0.9.4)
    , primToPrim
    , primToIO
    , primToST
    , module Data.Primitive.MutVar
      -- ** Debugging
    , trace
    , traceShow
    , traceId
    , traceM
    , traceShowId
    , traceShowM
      -- ** Time (since 0.6.1)
    , module Data.Time
#if MIN_VERSION_time(1,10,0)
    , parseTime
#endif
      -- ** Generics (since 0.8.1)
    , Generic
      -- ** Transformers (since 0.9.4)
    , Identity (..)
    , MonadReader
    , ask
    , asks
    , ReaderT (..)
    , Reader
      -- * Poly hierarchy
    , module Data.Foldable
    , module Data.Traversable
      -- ** Bifunctor (since 0.10.0)
    , module Data.Bifunctor
      -- * Mono hierarchy
    , module Data.MonoTraversable
    , module Data.MonoTraversable.Unprefixed
    , module Data.Sequences
    , module Data.Containers
    , module Data.Builder
    , module Data.NonNull
    , toByteVector
    , fromByteVector
      -- * I\/O
    , module Say
      -- * Concurrency
    , yieldThread
    , waitAsync
    , pollAsync
    , waitCatchAsync
    , linkAsync
    , link2Async
      -- * Non-standard
      -- ** List-like classes
    , map
    --, split
    , readMay
    , zip, zip3, zip4, zip5, zip6, zip7
    , unzip, unzip3, unzip4, unzip5, unzip6, unzip7
    , zipWith, zipWith3, zipWith4, zipWith5, zipWith6, zipWith7

    , hashNub
    , ordNub
    , ordNubBy

    , sortWith
    , Prelude.repeat
      -- ** Set-like
    , (\\)
    , intersect
    -- FIXME , mapSet
      -- ** Text-like
    , Show (..)
    , tshow
    , tlshow
      -- *** Case conversion
    , charToLower
    , charToUpper
      -- ** IO
    , readFile
    , readFileUtf8
    , writeFile
    , writeFileUtf8
    , hGetContents
    , hPut
    , hGetChunk
    , print
    -- Prelude IO operations
    , putChar
    , putStr
    , putStrLn
    , getChar
    , getLine
    , getContents
    , interact
      -- ** Difference lists
    , DList
    , asDList
    , applyDList
      -- ** Exceptions
    , module Control.DeepSeq
      -- ** Force types
      -- | Helper functions for situations where type inferer gets confused.
    , asByteString
    , asLByteString
    , asHashMap
    , asHashSet
    , asText
    , asLText
    , asList
    , asMap
    , asIntMap
    , asMaybe
    , asSet
    , asIntSet
    , asVector
    , asUVector
    , asSVector
    , asString
    ) where

import qualified Prelude
import Control.Applicative ((<**>),liftA,liftA2,liftA3,Alternative (..), optional)
import Data.Functor
import Control.Exception (assert)
import Control.DeepSeq (deepseq, ($!!), force, NFData (..))
import Control.Monad (when, unless, void, liftM, ap, forever, join, replicateM_, guard, MonadPlus (..), (=<<), (>=>), (<=<), liftM2, liftM3, liftM4, liftM5)
import qualified Control.Concurrent.STM as STM
import Data.Mutable
import Data.Traversable (Traversable (..), for, forM)
import Data.Foldable (Foldable)
import UnliftIO

import Data.Vector.Instances ()
import CorePrelude hiding
    ( putStr, putStrLn, print, undefined, (<>), catMaybes, first, second
    , catchIOError
    )
import Data.ChunkedZip
import qualified Data.Char as Char
import Data.Sequences
import Data.MonoTraversable
import Data.MonoTraversable.Unprefixed
import Data.MonoTraversable.Instances ()
import Data.Containers
import Data.Builder
import Data.NonNull
import qualified Data.ByteString
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Lazy.IO as LTextIO
import Data.ByteString.Internal (ByteString (PS))
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Data.Vector.Storable (unsafeToForeignPtr, unsafeFromForeignPtr)

import qualified Debug.Trace as Trace
import Data.Semigroup (Semigroup (..), WrappedMonoid (..))
import Prelude (Show (..))
import Data.Time
    ( UTCTime (..)
    , Day (..)
    , toGregorian
    , fromGregorian
    , formatTime
#if !MIN_VERSION_time(1,10,0)
    , parseTime
#endif
    , parseTimeM
    , getCurrentTime
    , defaultTimeLocale
    )
import qualified Data.Time as Time

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.HashSet as HashSet

import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)

import Control.Monad.Primitive (primToPrim, primToIO, primToST)
import Data.Primitive.MutVar

import Data.Functor.Identity (Identity (..))
import Control.Monad.Reader (MonadReader, ask, asks, ReaderT (..), Reader)
import Data.Bifunctor
import Data.DList (DList)
import qualified Data.DList as DList
import Say
import Control.Concurrent.STM.TBChan
import Control.Concurrent.STM.TBMChan
import Control.Concurrent.STM.TBMQueue
import Control.Concurrent.STM.TMChan
import Control.Concurrent.STM.TMQueue
import qualified Control.Concurrent

tshow :: Show a => a -> Text
tshow = fromList . Prelude.show

tlshow :: Show a => a -> LText
tlshow = fromList . Prelude.show

-- | Convert a character to lower case.
--
-- Character-based case conversion is lossy in comparison to string-based 'Data.MonoTraversable.toLower'.
-- For instance, &#x130; will be converted to i, instead of i&#x307;.
charToLower :: Char -> Char
charToLower = Char.toLower

-- | Convert a character to upper case.
--
-- Character-based case conversion is lossy in comparison to string-based 'Data.MonoTraversable.toUpper'.
-- For instance, &#xdf; won't be converted to SS.
charToUpper :: Char -> Char
charToUpper = Char.toUpper

readMay :: (Element c ~ Char, MonoFoldable c, Read a) => c -> Maybe a
readMay a = -- FIXME replace with safe-failure stuff
    case [x | (x, t) <- Prelude.reads (otoList a :: String), onull t] of
        [x] -> Just x
        _ -> Nothing

map :: Functor f => (a -> b) -> f a -> f b
map = fmap

infixr 5  ++
(++) :: Monoid m => m -> m -> m
(++) = mappend
{-# INLINE (++) #-}

infixl 9 \\{-This comment teaches CPP correct behaviour -}
-- | An alias for 'difference'.
(\\) :: SetContainer a => a -> a -> a
(\\) = difference
{-# INLINE (\\) #-}

-- | An alias for 'intersection'.
intersect :: SetContainer a => a -> a -> a
intersect = intersection
{-# INLINE intersect #-}

asByteString :: ByteString -> ByteString
asByteString = id

asLByteString :: LByteString -> LByteString
asLByteString = id

asHashMap :: HashMap k v -> HashMap k v
asHashMap = id

asHashSet :: HashSet a -> HashSet a
asHashSet = id

asText :: Text -> Text
asText = id

asLText :: LText -> LText
asLText = id

asList :: [a] -> [a]
asList = id

asMap :: Map k v -> Map k v
asMap = id

asIntMap :: IntMap v -> IntMap v
asIntMap = id

asMaybe :: Maybe a -> Maybe a
asMaybe = id

asSet :: Set a -> Set a
asSet = id

asIntSet :: IntSet -> IntSet
asIntSet = id

asVector :: Vector a -> Vector a
asVector = id

asUVector :: UVector a -> UVector a
asUVector = id

asSVector :: SVector a -> SVector a
asSVector = id

asString :: [Char] -> [Char]
asString = id

print :: (Show a, MonadIO m) => a -> m ()
print = liftIO . Prelude.print

-- | Sort elements using the user supplied function to project something out of
-- each element.
-- Inspired by <http://hackage.haskell.org/packages/archive/base/latest/doc/html/GHC-Exts.html#v:sortWith>.
sortWith :: (Ord a, IsSequence c) => (Element c -> a) -> c -> c
sortWith f = sortBy $ comparing f

-- | We define our own 'undefined' which is marked as deprecated. This makes it
-- useful to use during development, but lets you more easily get
-- notifications if you accidentally ship partial code in production.
--
-- The classy prelude recommendation for when you need to really have a partial
-- function in production is to use 'error' with a very descriptive message so
-- that, in case an exception is thrown, you get more information than
-- @"Prelude".'Prelude.undefined'@.
--
-- Since 0.5.5
undefined :: HasCallStack => a
undefined = error "ClassyPrelude.undefined"
{-# DEPRECATED undefined "It is highly recommended that you either avoid partial functions or provide meaningful error messages" #-}

-- | We define our own 'trace' (and also its variants) which provides a warning
-- when used. So that tracing is available during development, but the compiler
-- reminds you to not leave them in the code for production.
{-# WARNING trace "Leaving traces in the code" #-}
trace :: String -> a -> a
trace = Trace.trace

{-# WARNING traceShow "Leaving traces in the code" #-}
traceShow :: Show a => a -> b -> b
traceShow = Trace.traceShow

-- |
--
-- Since 0.5.9
{-# WARNING traceId "Leaving traces in the code" #-}
traceId :: String -> String
traceId a = Trace.trace a a

-- |
--
-- Since 0.5.9
{-# WARNING traceM "Leaving traces in the code" #-}
traceM :: (Monad m) => String -> m ()
traceM string = Trace.trace string $ return ()

-- |
--
-- Since 0.5.9
{-# WARNING traceShowId "Leaving traces in the code" #-}
traceShowId :: (Show a) => a -> a
traceShowId a = Trace.trace (show a) a

-- |
--
-- Since 0.5.9
{-# WARNING traceShowM "Leaving traces in the code" #-}
traceShowM :: (Show a, Monad m) => a -> m ()
traceShowM = traceM . show

-- | Originally 'Conc.yield'.
yieldThread :: MonadIO m => m ()
yieldThread = liftIO Control.Concurrent.yield
{-# INLINE yieldThread #-}

-- Below is a lot of coding for classy-prelude!
-- These functions are restricted to lists right now.
-- Should eventually exist in mono-foldable and be extended to MonoFoldable
-- when doing that should re-run the haskell-ordnub benchmarks

-- | same behavior as 'Data.List.nub', but requires 'Hashable' & 'Eq' and is @O(n log n)@
--
-- <https://github.com/nh2/haskell-ordnub>
hashNub :: (Hashable a, Eq a) => [a] -> [a]
hashNub = go HashSet.empty
  where
    go _ []     = []
    go s (x:xs) | x `HashSet.member` s = go s xs
                | otherwise            = x : go (HashSet.insert x s) xs

-- | same behavior as 'Data.List.nub', but requires 'Ord' and is @O(n log n)@
--
-- <https://github.com/nh2/haskell-ordnub>
ordNub :: (Ord a) => [a] -> [a]
ordNub = go Set.empty
  where
    go _ [] = []
    go s (x:xs) | x `Set.member` s = go s xs
                | otherwise        = x : go (Set.insert x s) xs

-- | same behavior as 'Data.List.nubBy', but requires 'Ord' and is @O(n log n)@
--
-- <https://github.com/nh2/haskell-ordnub>
ordNubBy :: (Ord b) => (a -> b) -> (a -> a -> Bool) -> [a] -> [a]
ordNubBy p f = go Map.empty
  -- When removing duplicates, the first function assigns the input to a bucket,
  -- the second function checks whether it is already in the bucket (linear search).
  where
    go _ []     = []
    go m (x:xs) = let b = p x in case b `Map.lookup` m of
                    Nothing     -> x : go (Map.insert b [x] m) xs
                    Just bucket
                      | elem_by f x bucket -> go m xs
                      | otherwise          -> x : go (Map.insert b (x:bucket) m) xs

    -- From the Data.List source code.
    elem_by :: (a -> a -> Bool) -> a -> [a] -> Bool
    elem_by _  _ []     = False
    elem_by eq y (x:xs) = y `eq` x || elem_by eq y xs

-- | Synonym for 'STM.orElse'.
orElseSTM :: STM a -> STM a -> STM a
orElseSTM = STM.orElse
{-# INLINE orElseSTM #-}

-- | Only perform the action if the predicate returns 'True'.
--
-- Since 0.9.2
whenM :: Monad m => m Bool -> m () -> m ()
whenM mbool action = mbool >>= flip when action

-- | Only perform the action if the predicate returns 'False'.
--
-- Since 0.9.2
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mbool action = mbool >>= flip unless action

-- | Force type to a 'DList'
--
-- Since 0.11.0
asDList :: DList a -> DList a
asDList = id
{-# INLINE asDList #-}

-- | Synonym for 'DList.apply'
--
-- Since 0.11.0
applyDList :: DList a -> [a] -> [a]
applyDList = DList.apply
{-# INLINE applyDList #-}

infixr 3 <&&>
-- | '&&' lifted to an Applicative.
--
-- @since 0.12.8
(<&&>) :: Applicative a => a Bool -> a Bool -> a Bool
(<&&>) = liftA2 (&&)
{-# INLINE (<&&>) #-}

infixr 2 <||>
-- | '||' lifted to an Applicative.
--
-- @since 0.12.8
(<||>) :: Applicative a => a Bool -> a Bool -> a Bool
(<||>) = liftA2 (||)
{-# INLINE (<||>) #-}

-- | Convert a 'ByteString' into a storable 'Vector'.
toByteVector :: ByteString -> SVector Word8
toByteVector (PS fptr offset idx) = unsafeFromForeignPtr fptr offset idx
{-# INLINE toByteVector #-}

-- | Convert a storable 'Vector' into a 'ByteString'.
fromByteVector :: SVector Word8 -> ByteString
fromByteVector v =
    PS fptr offset idx
  where
    (fptr, offset, idx) = unsafeToForeignPtr v
{-# INLINE fromByteVector #-}

-- | 'waitSTM' for any 'MonadIO'
--
-- @since 1.0.0
waitAsync :: MonadIO m => Async a -> m a
waitAsync = atomically . waitSTM

-- | 'pollSTM' for any 'MonadIO'
--
-- @since 1.0.0
pollAsync :: MonadIO m => Async a -> m (Maybe (Either SomeException a))
pollAsync = atomically . pollSTM

-- | 'waitCatchSTM' for any 'MonadIO'
--
-- @since 1.0.0
waitCatchAsync :: MonadIO m => Async a -> m (Either SomeException a)
waitCatchAsync = waitCatch

-- | 'Async.link' generalized to any 'MonadIO'
--
-- @since 1.0.0
linkAsync :: MonadIO m => Async a -> m ()
linkAsync = UnliftIO.link

-- | 'Async.link2' generalized to any 'MonadIO'
--
-- @since 1.0.0
link2Async :: MonadIO m => Async a -> Async b -> m ()
link2Async a = UnliftIO.link2 a

-- | Strictly read a file into a 'ByteString'.
--
-- @since 1.2.0
readFile :: MonadIO m => FilePath -> m ByteString
readFile = liftIO . Data.ByteString.readFile

-- | Strictly read a file into a 'Text' using a UTF-8 character
-- encoding. In the event of a character encoding error, a Unicode
-- replacement character will be used (a.k.a., @lenientDecode@).
--
-- @since 1.2.0
readFileUtf8 :: MonadIO m => FilePath -> m Text
readFileUtf8 = liftM decodeUtf8 . readFile

-- | Write a 'ByteString' to a file.
--
-- @since 1.2.0
writeFile :: MonadIO m => FilePath -> ByteString -> m ()
writeFile fp = liftIO . Data.ByteString.writeFile fp

-- | Write a 'Text' to a file using a UTF-8 character encoding.
--
-- @since 1.2.0
writeFileUtf8 :: MonadIO m => FilePath -> Text -> m ()
writeFileUtf8 fp = writeFile fp . encodeUtf8

-- | Strictly read the contents of the given 'Handle' into a
-- 'ByteString'.
--
-- @since 1.2.0
hGetContents :: MonadIO m => Handle -> m ByteString
hGetContents = liftIO . Data.ByteString.hGetContents

-- | Write a 'ByteString' to the given 'Handle'.
--
-- @since 1.2.0
hPut :: MonadIO m => Handle -> ByteString -> m ()
hPut h = liftIO . Data.ByteString.hPut h

-- | Read a single chunk of data as a 'ByteString' from the given
-- 'Handle'.
--
-- Under the surface, this uses 'Data.ByteString.hGetSome' with the
-- default chunk size.
--
-- @since 1.2.0
hGetChunk :: MonadIO m => Handle -> m ByteString
hGetChunk = liftIO . flip Data.ByteString.hGetSome defaultChunkSize

-- | Write a character to stdout
--
-- Uses system locale settings
--
-- @since 1.3.1
putChar :: MonadIO m => Char -> m ()
putChar = liftIO . Prelude.putChar

-- | Write a Text to stdout
--
-- Uses system locale settings
--
-- @since 1.3.1
putStr :: MonadIO m => Text -> m ()
putStr = liftIO . TextIO.putStr

-- | Write a Text followed by a newline to stdout
--
-- Uses system locale settings
--
-- @since 1.3.1
putStrLn :: MonadIO m => Text -> m ()
putStrLn = liftIO . TextIO.putStrLn

-- | Read a character from stdin
--
-- Uses system locale settings
--
-- @since 1.3.1
getChar :: MonadIO m => m Char
getChar = liftIO Prelude.getChar

-- | Read a line from stdin
--
-- Uses system locale settings
--
-- @since 1.3.1
getLine :: MonadIO m => m Text
getLine = liftIO TextIO.getLine

-- | Read all input from stdin into a lazy Text ('LText')
--
-- Uses system locale settings
--
-- @since 1.3.1
getContents :: MonadIO m => m LText
getContents = liftIO LTextIO.getContents

-- | Takes a function of type 'LText -> LText' and passes all input on stdin
-- to it, then prints result to stdout
--
-- Uses lazy IO
-- Uses system locale settings
--
-- @since 1.3.1
interact :: MonadIO m => (LText -> LText) -> m ()
interact = liftIO . LTextIO.interact


#if MIN_VERSION_time(1,10,0)
parseTime 
  :: Time.ParseTime t
  => Time.TimeLocale -- ^ Time locale.
  -> String -- ^ Format string.
  -> String -- ^ Input string.
  -> Maybe t -- ^ The time value, or 'Nothing' if the input could not be parsed using the given format.
parseTime = parseTimeM True
#endif


