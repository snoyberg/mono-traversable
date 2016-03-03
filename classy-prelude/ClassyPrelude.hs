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
      -- ** Monad
    , module Control.Monad
    , whenM
    , unlessM
      -- ** Mutable references
    , module Control.Concurrent.MVar.Lifted
    , module Control.Concurrent.Chan.Lifted
    , module Control.Concurrent.STM
    , atomically
    , alwaysSTM
    , alwaysSucceedsSTM
    , retrySTM
    , orElseSTM
    , checkSTM
    , module Data.IORef.Lifted
    , module Data.Mutable
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
    , assert
      -- ** Time (since 0.6.1)
    , module Data.Time
    , defaultTimeLocale
      -- ** Generics (since 0.8.1)
    , Generic
      -- ** Transformers (since 0.9.4)
    , Identity (..)
    , MonadReader
    , ask
    , ReaderT (..)
    , Reader
      -- * Poly hierarchy
    , module Data.Foldable
    , module Data.Traversable
      -- ** Bifunctor (since 0.10.0)
    , module Data.Bifunctor
      -- * Mono hierarchy
    , module Data.MonoTraversable
    , module Data.Sequences
    , module Data.Sequences.Lazy
    , module Data.Textual.Encoding
    , module Data.Containers
    , module Data.Builder
    , module Data.MinLen
    , module Data.ByteVector
      -- * I\/O
    , Handle
    , stdin
    , stdout
    , stderr
      -- * Non-standard
      -- ** List-like classes
    , map
    , concat
    , concatMap
    , foldMap
    , fold
    , length
    , null
    , pack
    , unpack
    , repack
    , toList
    , mapM_
    , traverse_
    , for_
    , sequence_
    , forM_
    , any
    , all
    , and
    , or
    , foldl'
    , foldr
    , foldM
    , elem
    --, split
    , readMay
    , intercalate
    , zip, zip3, zip4, zip5, zip6, zip7
    , unzip, unzip3, unzip4, unzip5, unzip6, unzip7
    , zipWith, zipWith3, zipWith4, zipWith5, zipWith6, zipWith7

    , hashNub
    , ordNub
    , ordNubBy

    , sortWith
    , compareLength
    , sum
    , product
    , Prelude.repeat
      -- ** Set-like
    , (\\)
    , intersect
    , unions
    -- FIXME , mapSet
      -- ** Text-like
    , Show (..)
    , tshow
    , tlshow
      -- *** Case conversion
    , charToLower
    , charToUpper
      -- ** IO
    , IOData (..)
    , print
    , hClose
      -- ** FilePath
    , fpToString
    , fpFromString
    , fpToText
    , fpFromText
    , fpToTextWarn
    , fpToTextEx
      -- ** Difference lists
    , DList
    , asDList
    , applyDList
      -- ** Exceptions
    , module Control.Exception.Enclosed
    , MonadThrow (throwM)
    , MonadCatch
    , MonadMask
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
import Control.Exception.Enclosed
import Control.Monad (when, unless, void, liftM, ap, forever, join, replicateM_, guard, MonadPlus (..), (=<<), (>=>), (<=<), liftM2, liftM3, liftM4, liftM5)
import Control.Concurrent.MVar.Lifted
import Control.Concurrent.Chan.Lifted
import Control.Concurrent.STM hiding (atomically, always, alwaysSucceeds, retry, orElse, check)
import qualified Control.Concurrent.STM as STM
import Data.IORef.Lifted
import Data.Mutable
import qualified Data.Monoid as Monoid
import Data.Traversable (Traversable (..), for, forM)
import Data.Foldable (Foldable)
import Data.IOData (IOData (..))
import Control.Monad.Catch (MonadThrow (throwM), MonadCatch, MonadMask)

import Data.Vector.Instances ()
import CorePrelude hiding (print, undefined, (<>), catMaybes, first, second)
import Data.ChunkedZip
import qualified Data.Char as Char
import Data.Sequences hiding (elem, intercalate)
import qualified Data.Sequences (intercalate)
import Data.MonoTraversable
import Data.Containers
import Data.Builder
import Data.MinLen
import Data.ByteVector
import System.IO (Handle, stdin, stdout, stderr, hClose)

import Debug.Trace (trace, traceShow)
import Data.Semigroup (Semigroup (..), WrappedMonoid (..))
import Prelude (Show (..))
import Data.Time
    ( UTCTime (..)
    , Day (..)
    , toGregorian
    , fromGregorian
    , formatTime
    , parseTime
    , getCurrentTime
    )
import Data.Time.Locale.Compat (defaultTimeLocale)

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.HashSet as HashSet

import Data.Textual.Encoding
import Data.Sequences.Lazy
import GHC.Generics (Generic)

import Control.Monad.Primitive (primToPrim, primToIO, primToST)
import Data.Primitive.MutVar

import Data.Functor.Identity (Identity (..))
import Control.Monad.Reader (MonadReader, ask, ReaderT (..), Reader)
import Data.Bifunctor
import Data.DList (DList)
import qualified Data.DList as DList

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

-- Renames from mono-traversable

pack :: IsSequence c => [Element c] -> c
pack = fromList

unpack, toList :: MonoFoldable c => c -> [Element c]
unpack = otoList
toList = otoList

null :: MonoFoldable c => c -> Bool
null = onull

compareLength :: (Integral i, MonoFoldable c) => c -> i -> Ordering
compareLength = ocompareLength

sum :: (MonoFoldable c, Num (Element c)) => c -> Element c
sum = osum

product :: (MonoFoldable c, Num (Element c)) => c -> Element c
product = oproduct

all :: MonoFoldable c => (Element c -> Bool) -> c -> Bool
all = oall
{-# INLINE all #-}

any :: MonoFoldable c => (Element c -> Bool) -> c -> Bool
any = oany
{-# INLINE any #-}

-- |
--
-- Since 0.9.2
and :: (MonoFoldable mono, Element mono ~ Bool) => mono -> Bool
and = oand
{-# INLINE and #-}

-- |
--
-- Since 0.9.2
or :: (MonoFoldable mono, Element mono ~ Bool) => mono -> Bool
or = oor
{-# INLINE or #-}

length :: MonoFoldable c => c -> Int
length = olength

-- Due to the Applicative-Monad-Proposal, from GHC 7.10 (base 4.8) we can
-- generalize some Monad constraints to Applicative constraints
#if MIN_VERSION_base(4,8,0)

mapM_ :: (Applicative f, MonoFoldable c) => (Element c -> f ()) -> c -> f ()
mapM_ = traverse_

forM_ :: (Applicative f, MonoFoldable c) => c -> (Element c -> f ()) -> f ()
forM_ = ofor_

#else

mapM_ :: (Monad m, MonoFoldable c) => (Element c -> m ()) -> c -> m ()
mapM_ = omapM_

forM_ :: (Monad m, MonoFoldable c) => c -> (Element c -> m ()) -> m ()
forM_ = oforM_

#endif

{-# INLINE mapM_ #-}
{-# INLINE forM_ #-}

traverse_ :: (Applicative f, MonoFoldable c) => (Element c -> f ()) -> c -> f ()
traverse_ = otraverse_
{-# INLINE traverse_ #-}

for_ :: (Applicative f, MonoFoldable c) => c -> (Element c -> f ()) -> f ()
for_ = ofor_
{-# INLINE for_ #-}

concatMap :: (Monoid m, MonoFoldable c) => (Element c -> m) -> c -> m
concatMap = ofoldMap
{-# INLINE concatMap #-}

elem :: (MonoFoldableEq c) => Element c -> c -> Bool
elem = oelem
{-# INLINE elem #-}

foldMap :: (Monoid m, MonoFoldable c) => (Element c -> m) -> c -> m
foldMap = ofoldMap
{-# INLINE foldMap #-}

fold :: (Monoid (Element c), MonoFoldable c) => c -> Element c
fold = ofoldMap id
{-# INLINE fold #-}

foldr :: MonoFoldable c => (Element c -> b -> b) -> b -> c -> b
foldr = ofoldr

foldl' :: MonoFoldable c => (a -> Element c -> a) -> a -> c -> a
foldl' = ofoldl'

foldM :: (Monad m, MonoFoldable c) => (a -> Element c -> m a) -> a -> c -> m a
foldM = ofoldlM

concat :: (MonoFoldable c, Monoid (Element c)) => c -> Element c
concat = ofoldMap id

readMay :: (Element c ~ Char, MonoFoldable c, Read a) => c -> Maybe a
readMay a = -- FIXME replace with safe-failure stuff
    case [x | (x, t) <- Prelude.reads (otoList a :: String), onull t] of
        [x] -> Just x
        _ -> Nothing

-- | Repack from one type to another, dropping to a list in the middle.
--
-- @repack = pack . unpack@.
repack :: (MonoFoldable a, IsSequence b, Element a ~ Element b) => a -> b
repack = fromList . toList

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

unions :: (MonoFoldable c, SetContainer (Element c)) => c -> Element c
unions = ofoldl' union Monoid.mempty

intercalate :: (MonoFoldable mono, Monoid (Element mono))
            => Element mono
            -> mono
            -> Element mono
intercalate x = mconcat . intersperse x . otoList
{-# INLINE [0] intercalate #-}
{-# RULES "intercalate list" forall (x :: [a]).
        intercalate x = Data.Sequences.intercalate x . toList #-}
{-# RULES "intercalate ByteString" forall (x :: ByteString).
        intercalate x = Data.Sequences.intercalate x . toList #-}
{-# RULES "intercalate LByteString" forall (x :: LByteString).
        intercalate x = Data.Sequences.intercalate x . toList #-}
{-# RULES "intercalate Text" forall (x :: Text).
        intercalate x = Data.Sequences.intercalate x . toList #-}
{-# RULES "intercalate LText" forall (x :: LText).
        intercalate x = Data.Sequences.intercalate x . toList #-}
{-# RULES "intercalate Seq" forall (x :: Seq a).
        intercalate x = Data.Sequences.intercalate x . toList #-}
{-# RULES "intercalate Vector" forall (x :: Vector a).
        intercalate x = Data.Sequences.intercalate x . toList #-}
{-# RULES "intercalate UVector" forall (x :: Unbox a => UVector a).
        intercalate x = Data.Sequences.intercalate x . toList #-}
{-# RULES "intercalate SVector" forall (x :: Storable a => SVector a).
        intercalate x = Data.Sequences.intercalate x . toList #-}

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
undefined :: a
undefined = error "ClassyPrelude.undefined"
{-# DEPRECATED undefined "It is highly recommended that you either avoid partial functions or provide meaningful error messages" #-}

-- |
--
-- Since 0.5.9
traceId :: String -> String
traceId a = trace a a

-- |
--
-- Since 0.5.9
traceM :: (Monad m) => String -> m ()
traceM string = trace string $ return ()

-- |
--
-- Since 0.5.9
traceShowId :: (Show a) => a -> a
traceShowId a = trace (show a) a

-- |
--
-- Since 0.5.9
traceShowM :: (Show a, Monad m) => a -> m ()
traceShowM = traceM . show

fpToString :: FilePath -> String
fpToString = id
{-# DEPRECATED fpToString "Now same as id" #-}

fpFromString :: String -> FilePath
fpFromString = id
{-# DEPRECATED fpFromString "Now same as id" #-}

-- | Translates a 'FilePath' to a 'Text'
--
-- Warns if there are non-unicode sequences in the file name
fpToTextWarn :: Monad m => FilePath -> m Text
fpToTextWarn = return . pack
{-# DEPRECATED fpToTextWarn "Use pack" #-}

-- | Translates a 'FilePath' to a 'Text'
--
-- Throws an exception if there are non-unicode
-- sequences in the file name
--
-- Use this to assert that you know
-- a filename will translate properly into a 'Text'.
-- If you created the filename, this should be the case.
fpToTextEx :: FilePath -> Text
fpToTextEx = pack
{-# DEPRECATED fpToTextEx "Use pack" #-}

-- | Translates a 'FilePath' to a 'Text'
-- This translation is not correct for a (unix) filename
-- which can contain arbitrary (non-unicode) bytes: those bytes will be discarded.
--
-- This means you cannot translate the 'Text' back to the original file name.
--
-- If you control or otherwise understand the filenames
-- and believe them to be unicode valid consider using 'fpToTextEx' or 'fpToTextWarn'
fpToText :: FilePath -> Text
fpToText = pack
{-# DEPRECATED fpToText "Use pack" #-}

fpFromText :: Text -> FilePath
fpFromText = unpack
{-# DEPRECATED fpFromText "Use unpack" #-}

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

-- | Generalized version of 'STM.atomically'.
atomically :: MonadIO m => STM a -> m a
atomically = liftIO . STM.atomically

-- | Synonym for 'STM.retry'.
retrySTM :: STM a
retrySTM = STM.retry
{-# INLINE retrySTM #-}

-- | Synonym for 'STM.always'.
alwaysSTM :: STM Bool -> STM ()
alwaysSTM = STM.always
{-# INLINE alwaysSTM #-}

-- | Synonym for 'STM.alwaysSucceeds'.
alwaysSucceedsSTM :: STM a -> STM ()
alwaysSucceedsSTM = STM.alwaysSucceeds
{-# INLINE alwaysSucceedsSTM #-}

-- | Synonym for 'STM.orElse'.
orElseSTM :: STM a -> STM a -> STM a
orElseSTM = STM.orElse
{-# INLINE orElseSTM #-}

-- | Synonym for 'STM.check'.
checkSTM :: Bool -> STM ()
checkSTM = STM.check
{-# INLINE checkSTM #-}

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

sequence_ :: (Monad m, MonoFoldable mono, Element mono ~ (m a)) => mono -> m ()
sequence_ = mapM_ (>> return ())
{-# INLINE sequence_ #-}

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
