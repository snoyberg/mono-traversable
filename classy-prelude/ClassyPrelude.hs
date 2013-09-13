{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude
    ( -- * CorePrelude
      module CorePrelude
    , Seq
    , undefined
      -- * Standard
      -- ** Monoid
    , empty
    , append
    , (++)
      -- ** Monad
    , module Control.Monad
      -- ** Mutable references
    , module Control.Concurrent.MVar.Lifted
    , module Data.IORef.Lifted
      -- ** Debugging
    , trace
    , traceShow
    , traceId
    , traceM
    , traceShowId
    , traceShowM
      -- * Non-standard
      -- ** List-like classes
    , map
    , concat
    , concatMap
    , filter
    , find
    , length
    , singleton
    , null
    , pack
    , unpack
    , repack
    , fromList
    , toList
    , mapM
    , mapM_
    , forM
    , forM_
    , replicateM
    , replicateM_
    , stripPrefix
    , isPrefixOf
    , stripSuffix
    , isSuffixOf
    , isInfixOf
    , break
    , span
    , dropWhile
    , takeWhile
    , any
    , all
    , splitAt, take, drop
    , fold
    , words
    , unwords
    , lines
    , unlines
    , split
    , reverse
    , readMay
    , replicate
    , intercalate
    , intersperse
    , encodeUtf8
    , decodeUtf8
    , subsequences
    , permutations
    , partition
    , zip, zip3, zip4, zip5, zip6, zip7
    , unzip, unzip3, unzip4, unzip5, unzip6, unzip7
    , zipWith, zipWith3, zipWith4, zipWith5, zipWith6, zipWith7
    , nub
    , nubBy
    , sort
    , sortBy
    , sortWith
    , group
    , group'
    , groupBy
    , groupBy'
    , groupWith
    , cons
    , uncons
    , compareLength
    , Foldable.sum
    , Foldable.product
    , repeat
      -- ** Map-like
    , lookup
    , insert
    , delete
      -- ** Set-like
    , member
    , notMember
    , elem
    , notElem
    , union
    , difference
    , (\\)
    , intersection
    , intersect
    , unions
      -- ** Text-like
    , show
    , toLower
    , toUpper
    , toCaseFold
    , toStrict
    , fromStrict
      -- ** IO
    , readFile
    , writeFile
    , getLine
    , print
      -- ** Chunking
    , toChunks
    , fromChunks
      -- ** Exceptions
    , catchAny
    , handleAny
    , tryAny
    , catchAnyDeep
    , handleAnyDeep
    , tryAnyDeep
    , catchIO
    , handleIO
    , tryIO
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
    , asMaybe
    , asSet
    , asVector
    , asUVector  
    , asIOException
    , asSomeException
    ) where

import qualified Prelude
import Control.Monad (when, unless, void, liftM, ap, forever, join, sequence, sequence_, replicateM_)
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseWith, restoreM)
import Control.Concurrent.Async (withAsync, waitCatch)
import Control.Concurrent.MVar.Lifted
import Data.IORef.Lifted
import Data.Monoid (Monoid)
import qualified Data.Monoid as Monoid
import Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import Control.DeepSeq (NFData, ($!!))

import CorePrelude hiding (print, undefined)
import ClassyPrelude.Classes

import ClassyPrelude.ByteString ()
import ClassyPrelude.Char ()
import ClassyPrelude.Classes ()
import ClassyPrelude.FilePath ()
import ClassyPrelude.HashMap ()
import ClassyPrelude.HashSet ()
import ClassyPrelude.LByteString ()
import ClassyPrelude.LText ()
import ClassyPrelude.List ()
import ClassyPrelude.Map ()
import ClassyPrelude.Maybe ()
import ClassyPrelude.Set ()
import ClassyPrelude.Text ()
import ClassyPrelude.Vector ()
import ClassyPrelude.UVector ()
import ClassyPrelude.Sequence (Seq)

import Debug.Trace (trace, traceShow)

show :: (Show a, CanPack c Char) => a -> c
show = pack . Prelude.show

fromList :: CanPack c i => [i] -> c
fromList = pack

toList :: CanPack c i => c -> [i]
toList = unpack

readMay :: (Read b, CanPack a Char) => a -> Maybe b
readMay a =
    case [x | (x, t) <- Prelude.reads (unpack a), null t] of
        [x] -> Just x
        _ -> Nothing

-- | Repack from one type to another, dropping to a list in the middle.
--
-- @repack = pack . unpack@.
repack :: (CanPack a i, CanPack b i) => a -> b
repack = pack . unpack

append :: Monoid m => m -> m -> m
append = mappend
{-# INLINE append #-}

infixr 5  ++
(++) :: Monoid m => m -> m -> m
(++) = mappend
{-# INLINE (++) #-}

infixl 9 \\{-This comment teaches CPP correct behaviour -}
-- | An alias for `difference`.
(\\) :: CanDifference c => c -> c -> c
(\\) = difference
{-# INLINE (\\) #-}

-- | An alias for `intersection`.
intersect :: CanIntersection c => c -> c -> c
intersect = intersection
{-# INLINE intersect #-}

unions :: (Foldable cc, Monoid c, CanUnion c) => cc c -> c
unions = Foldable.foldl' union Monoid.mempty

intercalate :: (CanConcat c i, CanIntersperse c i) => i -> c -> i
intercalate xs xss = concat (intersperse xs xss)

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

asMaybe :: Maybe a -> Maybe a
asMaybe = id

asSet :: Set a -> Set a
asSet = id

asVector :: Vector a -> Vector a
asVector = id

asUVector :: UVector a -> UVector a
asUVector = id

forM :: CanMapM ci mco m i o => ci -> (i -> m o) -> mco
forM = flip mapM

forM_ :: (Monad m, CanMapM_ ci i) => ci -> (i -> m o) -> m ()
forM_ = flip mapM_

-- | An alias for 'member'
elem :: CanMember c k => k -> c -> Bool
elem = member

-- | An alias for 'notMember'
notElem :: CanMember c k => k -> c -> Bool
notElem = notMember

print :: (Show a, MonadIO m) => a -> m ()
print = liftIO . Prelude.print

take :: CanSplitAt c i => i -> c -> c
take i c  = Prelude.fst (splitAt i c)

drop :: CanSplitAt c i => i -> c -> c
drop i c  = Prelude.snd (splitAt i c)

-- | Sort elements using the user supplied function to project something out of
-- each element.
-- Inspired by <http://hackage.haskell.org/packages/archive/base/latest/doc/html/GHC-Exts.html#v:sortWith>.
sortWith :: (CanSortBy c a, Ord b) => (a -> b) -> c -> c
sortWith f = sortBy $ comparing f

-- | The 'groupWith' function uses the user supplied function which
-- projects an element out of every list element in order to first sort the
-- input list and then to form groups by equality on these projected elements
--
-- Inspired by <http://hackage.haskell.org/packages/archive/base/latest/doc/html/GHC-Exts.html#v:groupWith>
groupWith :: (CanGroupBy' c a, Eq b) => (a -> b) -> c -> [c]
groupWith f = groupBy' (\a b -> f a == f b)

-- | We define our own @undefined@ which is marked as deprecated. This makes it
-- useful to use during development, but let's you more easily getting
-- notification if you accidentally ship partial code in production.
--
-- The classy prelude recommendation for when you need to really have a partial
-- function in production is to use @error@ with a very descriptive message so
-- that, in case an exception is thrown, you get more information than
-- @Prelude.undefined@.
--
-- Since 0.5.5
undefined :: a
undefined = error "ClassyPrelude.undefined"
{-# DEPRECATED undefined "It is highly recommended that you either avoid partial functions or provide meaningful error messages" #-}

-- | A version of 'catch' which is specialized for any exception. This
-- simplifies usage as no explicit type signatures are necessary.
--
-- Note that since version 0.5.9, this function now has proper support for
-- asynchronous exceptions, by only catching exceptions generated by the
-- internal action.
--
-- Since 0.5.6
catchAny :: MonadBaseControl IO m => m a -> (SomeException -> m a) -> m a
catchAny action onE = tryAny action >>= either onE return

-- | A version of 'handle' which is specialized for any exception.  This
-- simplifies usage as no explicit type signatures are necessary.
--
-- Note that since version 0.5.9, this function now has proper support for
-- asynchronous exceptions, by only catching exceptions generated by the
-- internal action.
--
-- Since 0.5.6
handleAny :: MonadBaseControl IO m => (SomeException -> m a) -> m a -> m a
handleAny = flip catchAny

-- | A version of 'try' which is specialized for any exception.
-- This simplifies usage as no explicit type signatures are necessary.
--
-- Note that since version 0.5.9, this function now has proper support for
-- asynchronous exceptions, by only catching exceptions generated by the
-- internal action.
--
-- Since 0.5.6
tryAny :: MonadBaseControl IO m => m a -> m (Either SomeException a)
tryAny m =
    liftBaseWith (\runInIO -> withAsync (runInIO m) waitCatch) >>=
    either (return . Left) (liftM Right . restoreM)

-- | An extension to @catchAny@ which ensures that the return value is fully
-- evaluated. See @tryAnyDeep@.
--
-- Since 0.5.9
catchAnyDeep :: (NFData a, MonadBaseControl IO m) => m a -> (SomeException -> m a) -> m a
catchAnyDeep action onE = tryAnyDeep action >>= either onE return

-- | @flip catchAnyDeep@
--
-- Since 0.5.6
handleAnyDeep :: (NFData a, MonadBaseControl IO m) => (SomeException -> m a) -> m a -> m a
handleAnyDeep = flip catchAnyDeep

-- | An extension to @tryAny@ which ensures that the return value is fully
-- evaluated. In other words, if you get a @Right@ response here, you can be
-- confident that using it will not result in another exception.
--
-- Since 0.5.9
tryAnyDeep :: (NFData a, MonadBaseControl IO m)
           => m a
           -> m (Either SomeException a)
tryAnyDeep m = tryAny $ do
    x <- m
    return $!! x

-- | A version of 'catch' which is specialized for IO exceptions. This
-- simplifies usage as no explicit type signatures are necessary.
--
-- Since 0.5.6
catchIO :: MonadBaseControl IO m => m a -> (IOException -> m a) -> m a
catchIO = catch

-- | A version of 'handle' which is specialized for IO exceptions.  This
-- simplifies usage as no explicit type signatures are necessary.
--
-- Since 0.5.6
handleIO :: MonadBaseControl IO m => (IOException -> m a) -> m a -> m a
handleIO = handle

-- | A version of 'try' which is specialized for IO exceptions.
-- This simplifies usage as no explicit type signatures are necessary.
--
-- Since 0.5.6
tryIO :: MonadBaseControl IO m => m a -> m (Either IOException a)
tryIO = try

-- |
--
-- Since 0.5.6
asSomeException :: SomeException -> SomeException
asSomeException = id

-- |
--
-- Since 0.5.6
asIOException :: IOException -> IOException
asIOException = id

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
