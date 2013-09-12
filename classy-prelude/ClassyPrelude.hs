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
      -- * Mono hierarchy
    , module Data.MonoTraversable
    , module Data.Sequences
    , module Data.Containers
      -- * Non-standard
      -- ** List-like classes
    , map
    , concat
    , concatMap
    , length
    , null
    , pack
    , unpack
    , repack
    , toList
    , Traversable.mapM
    , mapM_
    , Traversable.forM
    , forM_
    , any
    , all
    , foldl'
    , foldr
    --, split
    , readMay
    , intercalate
    , zip, zip3, zip4, zip5, zip6, zip7
    , unzip, unzip3, unzip4, unzip5, unzip6, unzip7
    , zipWith, zipWith3, zipWith4, zipWith5, zipWith6, zipWith7
    {-
    , nub
    , nubBy
    -}
    , sortWith
    , groupWith
    , compareLength
    , sum
    , product
    , Prelude.repeat
      -- ** Set-like
    , (\\)
    , intersect
    , unions
      -- ** Text-like
    , show
      -- ** IO
    , readFile
    , writeFile
    , getLine
    , print
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
import qualified Data.Monoid as Monoid
import qualified Data.Traversable as Traversable
import Control.DeepSeq (NFData, ($!!))

import CorePrelude hiding (print, undefined)
import ClassyPrelude.Classes
import Data.Sequences
import Data.MonoTraversable
import Data.Containers

import Debug.Trace (trace, traceShow)

show :: (IsSequence c, Element c ~ Char, Show a) => a -> c
show = fromList . Prelude.show

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

any :: MonoFoldable c => (Element c -> Bool) -> c -> Bool
any = oany

length :: MonoFoldable c => c -> Int
length = olength

mapM_ :: (Monad m, MonoFoldable c) => (Element c -> m a) -> c -> m ()
mapM_ = omapM_

forM_ :: (Monad m, MonoFoldable c) => c -> (Element c -> m a) -> m ()
forM_ = oforM_

concatMap :: MonoFoldableMonoid c => (Element c -> c) -> c -> c
concatMap = oconcatMap

foldr :: MonoFoldable c => (Element c -> b -> b) -> b -> c -> b
foldr = ofoldr

foldl' :: MonoFoldable c => (a -> Element c -> a) -> a -> c -> a
foldl' = ofoldl'

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
-- | An alias for `difference`.
(\\) :: Container a => a -> a -> a
(\\) = difference
{-# INLINE (\\) #-}

-- | An alias for `intersection`.
intersect :: Container a => a -> a -> a
intersect = intersection
{-# INLINE intersect #-}

unions :: (MonoFoldable c, Container (Element c)) => c -> Element c
unions = ofoldl' union Monoid.mempty

intercalate :: (Monoid (Element c), IsSequence c) => Element c -> c -> Element c
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

print :: (Show a, MonadIO m) => a -> m ()
print = liftIO . Prelude.print

-- | Sort elements using the user supplied function to project something out of
-- each element.
-- Inspired by <http://hackage.haskell.org/packages/archive/base/latest/doc/html/GHC-Exts.html#v:sortWith>.
sortWith :: (Ord a, IsSequence c) => (Element c -> a) -> c -> c
sortWith f = sortBy $ comparing f

-- | The 'groupWith' function uses the user supplied function which
-- projects an element out of every list element in order to first sort the
-- input list and then to form groups by equality on these projected elements
--
-- Inspired by <http://hackage.haskell.org/packages/archive/base/latest/doc/html/GHC-Exts.html#v:groupWith>
groupWith :: (IsSequence c, Eq a) => (Element c -> a) -> c -> [c]
groupWith f = groupBy (\a b -> f a == f b)

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

-- FIXME export toFilePath, fromFilePath
-- FIXME export Handle, stdout, stderr
-- FIXME mapSet
