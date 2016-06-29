{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | The functions in "Data.MonoTraversable" are all prefixed with the letter
-- @o@ to avoid conflicts with their polymorphic counterparts. This module
-- exports the same identifiers without the prefix, for all cases where the
-- monomorphic variant loses no generality versus the polymorphic version. For
-- example, 'olength' is just as general as @Data.Foldable.length@, so we
-- export @length = length@. By contrast, 'omap' cannot fully subsume @fmap@ or
-- @map@, so we do not provide such an export.
--
-- @since 1.0.0
module Data.MonoTraversable.Unprefixed where

import Data.Int (Int64)
import Data.MonoTraversable
import Data.Semigroup (Semigroup)
import Data.Monoid (Monoid)
import Control.Applicative (Applicative)

-- | Synonym for 'ofoldMap'
--
-- @since 1.0.0
foldMap :: (MonoFoldable mono, Data.Monoid.Monoid m) => (Element mono -> m) -> mono -> m
foldMap = ofoldMap

-- | Synonym for 'ofoldr'
--
-- @since 1.0.0
foldr :: MonoFoldable mono => (Element mono -> b -> b) -> b -> mono -> b
foldr = ofoldr

-- | Synonym for 'ofoldl''
--
-- @since 1.0.0
foldl' :: MonoFoldable mono => (a -> Element mono -> a) -> a -> mono -> a
foldl' = ofoldl'

-- | Synonym for 'otoList'
--
-- @since 1.0.0
toList :: MonoFoldable mono => mono -> [Element mono]
toList = otoList

-- | Synonym for 'oall'
--
-- @since 1.0.0
all :: MonoFoldable mono => (Element mono -> Bool) -> mono -> Bool
all = oall

-- | Synonym for 'oany'
--
-- @since 1.0.0
any :: MonoFoldable mono => (Element mono -> Bool) -> mono -> Bool
any = oany

-- | Synonym for 'onull'
--
-- @since 1.0.0
null :: MonoFoldable mono => mono -> Bool
null = onull

-- | Synonym for 'olength'
--
-- @since 1.0.0
length :: MonoFoldable mono => mono -> Int
length = olength

-- | Synonym for 'olength64'
--
-- @since 1.0.0
length64 :: MonoFoldable mono => mono -> Int64
length64 = olength64

-- | Synonym for 'ocompareLength'
--
-- @since 1.0.0
compareLength :: (MonoFoldable mono, Integral i) => mono -> i -> Ordering
compareLength = ocompareLength

-- | Synonym for 'otraverse_'
--
-- @since 1.0.0
traverse_ :: (MonoFoldable mono, Control.Applicative.Applicative f) => (Element mono -> f b) -> mono -> f ()
traverse_ = otraverse_

-- | Synonym for 'ofor_'
--
-- @since 1.0.0
for_ :: (MonoFoldable mono, Applicative f) => mono -> (Element mono -> f b) -> f ()
for_ = ofor_

-- | Synonym for 'omapM_'
--
-- @since 1.0.0
#if MIN_VERSION_base(4,8,0)
mapM_ :: (MonoFoldable mono, Applicative m)
      => (Element mono -> m ()) -> mono -> m ()
#else
mapM_ :: (MonoFoldable mono, Monad m)
      => (Element mono -> m ()) -> mono -> m ()
#endif
mapM_ = omapM_

-- | Synonym for 'oforM_'
--
-- @since 1.0.0
#if MIN_VERSION_base(4,8,0)
forM_ :: (MonoFoldable mono, Applicative m)
      => mono -> (Element mono -> m ()) -> m ()
#else
forM_ :: (MonoFoldable mono, Monad m)
      => mono -> (Element mono -> m ()) -> m ()
#endif
forM_ = oforM_

-- | Synonym for 'ofoldlM'
--
-- @since 1.0.0
foldlM :: (MonoFoldable mono, Monad m)
       => (a -> Element mono -> m a)
       -> a
       -> mono
       -> m a
foldlM = ofoldlM

-- | Synonym for 'ofoldMap1Ex'
--
-- @since 1.0.0
foldMap1Ex :: (MonoFoldable mono, Semigroup m)
           => (Element mono -> m)
           -> mono
           -> m
foldMap1Ex = ofoldMap1Ex

-- | Synonym for 'ofoldr1Ex'
--
-- @since 1.0.0
foldr1Ex :: MonoFoldable mono
         => (Element mono -> Element mono -> Element mono)
         -> mono
         -> Element mono
foldr1Ex = ofoldr1Ex

-- | Synonym for 'ofoldl1Ex''
--
-- @since 1.0.0
foldl1Ex' :: MonoFoldable mono
          => (Element mono -> Element mono -> Element mono)
          -> mono
          -> Element mono
foldl1Ex' = ofoldl1Ex'

-- | Synonym for 'osum'
--
-- @since 1.0.0
sum :: (MonoFoldable mono, Num (Element mono)) => mono -> Element mono
sum = osum

-- | Synonym for 'oproduct'
--
-- @since 1.0.0
product :: (MonoFoldable mono, Num (Element mono)) => mono -> Element mono
product = oproduct

-- | Synonym for 'oand'
--
-- @since 1.0.0
and :: (MonoFoldable mono, Element mono ~ Bool) => mono -> Bool
and = oand

-- | Synonym for 'oor'
--
-- @since 1.0.0
or :: (MonoFoldable mono, Element mono ~ Bool) => mono -> Bool
or = oor

-- | Synonym for 'oconcatMap'
--
-- @since 1.0.0
concatMap :: (MonoFoldable mono, Monoid m) => (Element mono -> m) -> mono -> m
concatMap = oconcatMap

-- | Synonym for 'oelem'
--
-- @since 1.0.0
elem :: (MonoFoldable mono, Eq (Element mono)) => Element mono -> mono -> Bool
elem = oelem

-- | Synonym for 'onotElem'
--
-- @since 1.0.0
notElem :: (MonoFoldable mono, Eq (Element mono)) => Element mono -> mono -> Bool
notElem = onotElem

-- | Synonym for 'opoint'
--
-- @since 1.0.0
point :: MonoPointed mono => Element mono -> mono
point = opoint

-- | Synonym for 'ointercalate'
--
-- @since 1.0.0
intercalate :: (MonoFoldable mono, Monoid (Element mono))
            => Element mono -> mono -> Element mono
intercalate = ointercalate

-- | Synonym for 'ofold'
--
-- @since 1.0.0
fold :: (MonoFoldable mono, Monoid (Element mono)) => mono -> Element mono
fold = ofold

-- | Synonym for 'oconcat'
--
-- @since 1.0.0
concat :: (MonoFoldable mono, Monoid (Element mono)) => mono -> Element mono
concat = oconcat

-- | Synonym for 'ofoldM'
--
-- @since 1.0.0
foldM :: (MonoFoldable mono, Monad m) => (a -> Element mono -> m a) -> a -> mono -> m a
foldM = ofoldM

-- | Synonym for 'osequence_'
--
-- @since 1.0.0
#if MIN_VERSION_base(4,8,0)
sequence_ :: (Applicative m, MonoFoldable mono, Element mono ~ (m ())) => mono -> m ()
#else
sequence_ :: (Monad m, MonoFoldable mono, Element mono ~ (m ())) => mono -> m ()
#endif
sequence_ = osequence_
