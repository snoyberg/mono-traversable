{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Warning: This module should be considered highly experimental.
module Data.Sequences where

import Data.Monoid (Monoid, mconcat, mempty)
import Data.MonoTraversable
import Data.Int (Int64, Int)
import qualified Data.List as List
import qualified Control.Monad (filterM, replicateM)
import Prelude (Bool (..), Monad (..), Maybe (..), Ordering (..), Ord (..), Eq (..), Functor (..), fromIntegral, otherwise, (-), not, fst, snd, Integral, ($), flip)
import Data.Char (Char, isSpace)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Category
import Control.Arrow ((***), second)
import Control.Monad (liftM)
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as VS
import GHC.Exts (Constraint)
import qualified Data.Set as Set
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.String (IsString)
import qualified Data.List.NonEmpty as NE

-- | 'SemiSequence' was created to share code between 'IsSequence' and 'NonNull'.
-- You should always use 'IsSequence' or 'NonNull' rather than using 'SemiSequence'
-- 'SemiSequence' is exported so that you can define new instances of 'IsSequence' or 'NonNull'
--
-- @Semi@ means 'SemiGroup'
-- A 'SemiSequence' can accomodate a 'SemiGroup' such as 'NonEmpty'
-- A Monoid should be able to fill out 'IsSequence'
--
-- As a base for 'NonNull',
-- a 'SemiSequence' keeps the same type when increasing its number of elements.
-- However, a decreasing function such as filter may change a 'NonNull' type.
-- For example, from 'NonEmpty' to '[]'
-- This exists on 'NonNull' as 'nfilter'
--
-- 'filter' and other such functions are placed in 'IsSequence'
class (Integral (Index seq)) => SemiSequence seq where
    type Index seq
    singleton :: Element seq -> seq

    intersperse :: Element seq -> seq -> seq

    -- FIXME split :: (Element seq -> Bool) -> seq -> [seq]

    reverse :: seq -> seq

    find :: (Element seq -> Bool) -> seq -> Maybe (Element seq)

    sortBy :: (Element seq -> Element seq -> Ordering) -> seq -> seq

    cons :: Element seq -> seq -> seq

    snoc :: seq -> Element seq -> seq


-- | Sequence Laws:
--
-- > fromList . otoList = id
-- > fromList (x <> y) = fromList x <> fromList y
-- > otoList (fromList x <> fromList y) = x <> y
class (Monoid seq, MonoTraversable seq, SemiSequence seq) => IsSequence seq where
    fromList :: [Element seq] -> seq
    -- this definition creates the Monoid constraint
    -- However, all the instances define their own fromList
    fromList = mconcat . fmap singleton

    -- below functions change type fron the perspective of NonEmpty
    break :: (Element seq -> Bool) -> seq -> (seq, seq)
    break f = (fromList *** fromList) . List.break f . otoList

    span :: (Element seq -> Bool) -> seq -> (seq, seq)
    span f = (fromList *** fromList) . List.span f . otoList

    dropWhile :: (Element seq -> Bool) -> seq -> seq
    dropWhile f = fromList . List.dropWhile f . otoList
    
    takeWhile :: (Element seq -> Bool) -> seq -> seq
    takeWhile f = fromList . List.takeWhile f . otoList

    splitAt :: Index seq -> seq -> (seq, seq)
    splitAt i = (fromList *** fromList) . List.genericSplitAt i . otoList

    take :: Index seq -> seq -> seq
    take i = fst . splitAt i

    drop :: Index seq -> seq -> seq
    drop i = snd . splitAt i

    partition :: (Element seq -> Bool) -> seq -> (seq, seq)
    partition f = (fromList *** fromList) . List.partition f . otoList
    
    uncons :: seq -> Maybe (Element seq, seq)
    uncons = fmap (second fromList) . uncons . otoList

    unsnoc :: seq -> Maybe (seq, Element seq)
    unsnoc seq =
        case reverse (otoList seq) of
            [] -> Nothing
            x:xs -> Just (fromList (reverse xs), x)

    filter :: (Element seq -> Bool) -> seq -> seq
    filter f = fromList . List.filter f . otoList

    filterM :: Monad m => (Element seq -> m Bool) -> seq -> m seq
    filterM f = liftM fromList . filterM f . otoList

    -- replicates are not in SemiSequence to allow for zero
    replicate :: Index seq -> Element seq -> seq
    replicate i = fromList . List.genericReplicate i

    replicateM :: Monad m => Index seq -> m (Element seq) -> m seq
    replicateM i = liftM fromList . Control.Monad.replicateM (fromIntegral i)

    -- below functions are not in SemiSequence because they return a List (instead of NonEmpty)
    groupBy :: (Element seq -> Element seq -> Bool) -> seq -> [seq]
    groupBy f = fmap fromList . List.groupBy f . otoList

    -- | Similar to standard 'groupBy', but operates on the whole collection, 
    -- not just the consecutive items.
    groupAllOn :: Eq b => (Element seq -> b) -> seq -> [seq]
    groupAllOn f = fmap fromList . groupAllOn f . otoList

    subsequences :: seq -> [seq]
    subsequences = List.map fromList . List.subsequences . otoList

    permutations :: seq -> [seq]
    permutations = List.map fromList . List.permutations . otoList


defaultFind :: MonoFoldable seq => (Element seq -> Bool) -> seq -> Maybe (Element seq)
defaultFind f = List.find f . otoList

defaultIntersperse :: IsSequence seq => Element seq -> seq -> seq
defaultIntersperse e = fromList . List.intersperse e . otoList

defaultReverse :: IsSequence seq => seq -> seq
defaultReverse = fromList . List.reverse . otoList

defaultSortBy :: IsSequence seq => (Element seq -> Element seq -> Ordering) -> seq -> seq
defaultSortBy f = fromList . List.sortBy f . otoList

defaultCons :: IsSequence seq => Element seq -> seq -> seq
defaultCons e = fromList . (e:) . otoList

defaultSnoc :: IsSequence seq => seq -> Element seq -> seq
defaultSnoc seq e = fromList (otoList seq List.++ [e])


-- | like Data.List.head, but not partial
headMay :: IsSequence seq => seq -> Maybe (Element seq)
headMay = fmap fst . uncons

-- | like Data.List.last, but not partial
lastMay :: IsSequence seq => seq -> Maybe (Element seq)
lastMay = fmap snd . unsnoc

-- | like Data.List.tail, but an input of @mempty@ returns @mempty@
tailDef :: IsSequence seq => seq -> seq
tailDef xs = case uncons xs of
               Nothing -> mempty
               Just tuple -> snd tuple

-- | like Data.List.init, but an input of @mempty@ returns @mempty@
initDef :: IsSequence seq => seq -> seq
initDef xs = case unsnoc xs of
               Nothing -> mempty
               Just tuple -> fst tuple



instance SemiSequence [a] where
    type Index [a] = Int
    singleton = return
    intersperse = List.intersperse
    reverse = List.reverse
    find = List.find
    sortBy = List.sortBy
    cons = (:)
    snoc = defaultSnoc

instance IsSequence [a] where
    fromList = id
    {-# INLINE fromList #-}
    filter = List.filter
    filterM = Control.Monad.filterM
    break = List.break
    span = List.span
    dropWhile = List.dropWhile
    takeWhile = List.takeWhile
    splitAt = List.splitAt
    take = List.take
    drop = List.drop
    uncons [] = Nothing
    uncons (x:xs) = Just (x, xs)
    partition = List.partition
    replicate = List.replicate
    replicateM = Control.Monad.replicateM
    groupBy = List.groupBy
    groupAllOn f (head : tail) =
        (head : matches) : groupAllOn f nonMatches
      where
        (matches, nonMatches) = partition ((== f head) . f) tail
    groupAllOn _ [] = []

instance SemiSequence (NE.NonEmpty a) where
    type Index (NE.NonEmpty a) = Int

    singleton    = (NE.:| [])
    intersperse  = NE.intersperse
    reverse      = NE.reverse
    find         = find
    cons         = NE.cons
    snoc xs x    = NE.fromList $ flip snoc x $ NE.toList xs
    sortBy f     = NE.fromList . List.sortBy f . NE.toList

instance SemiSequence S.ByteString where
    type Index S.ByteString = Int
    singleton = S.singleton
    intersperse = S.intersperse
    reverse = S.reverse
    find = S.find
    cons = S.cons
    snoc = S.snoc
    sortBy = defaultSortBy

instance IsSequence S.ByteString where
    fromList = S.pack
    replicate = S.replicate
    filter = S.filter
    break = S.break
    span = S.span
    dropWhile = S.dropWhile
    takeWhile = S.takeWhile
    splitAt = S.splitAt
    take = S.take
    drop = S.drop
    partition = S.partition
    uncons = S.uncons
    unsnoc s
        | S.null s = Nothing
        | otherwise = Just (S.init s, S.last s)
    groupBy = S.groupBy

instance SemiSequence T.Text where
    type Index T.Text = Int
    singleton = T.singleton
    intersperse = T.intersperse
    reverse = T.reverse
    find = T.find
    cons = T.cons
    snoc = T.snoc
    sortBy = defaultSortBy

instance IsSequence T.Text where
    fromList = T.pack
    replicate i c = T.replicate i (T.singleton c)
    filter = T.filter
    break = T.break
    span = T.span
    dropWhile = T.dropWhile
    takeWhile = T.takeWhile
    splitAt = T.splitAt
    take = T.take
    drop = T.drop
    partition = T.partition
    uncons = T.uncons
    unsnoc t
        | T.null t = Nothing
        | otherwise = Just (T.init t, T.last t)
    groupBy = T.groupBy

instance SemiSequence L.ByteString where
    type Index L.ByteString = Int64
    singleton = L.singleton
    intersperse = L.intersperse
    reverse = L.reverse
    find = L.find
    cons = L.cons
    snoc = L.snoc
    sortBy = defaultSortBy

instance IsSequence L.ByteString where
    fromList = L.pack
    replicate = L.replicate
    filter = L.filter
    break = L.break
    span = L.span
    dropWhile = L.dropWhile
    takeWhile = L.takeWhile
    splitAt = L.splitAt
    take = L.take
    drop = L.drop
    partition = L.partition
    uncons = L.uncons
    unsnoc s
        | L.null s = Nothing
        | otherwise = Just (L.init s, L.last s)
    groupBy = L.groupBy

instance SemiSequence TL.Text where
    type Index TL.Text = Int64
    singleton = TL.singleton
    intersperse = TL.intersperse
    reverse = TL.reverse
    find = TL.find
    cons = TL.cons
    snoc = TL.snoc
    sortBy = defaultSortBy

instance IsSequence TL.Text where
    fromList = TL.pack
    replicate i c = TL.replicate i (TL.singleton c)
    filter = TL.filter
    break = TL.break
    span = TL.span
    dropWhile = TL.dropWhile
    takeWhile = TL.takeWhile
    splitAt = TL.splitAt
    take = TL.take
    drop = TL.drop
    partition = TL.partition
    uncons = TL.uncons
    unsnoc t
        | TL.null t = Nothing
        | otherwise = Just (TL.init t, TL.last t)
    groupBy = TL.groupBy

instance SemiSequence (Seq.Seq a) where
    type Index (Seq.Seq a) = Int
    singleton = Seq.singleton
    cons = (Seq.<|)
    snoc = (Seq.|>)
    reverse = Seq.reverse
    sortBy = Seq.sortBy

    intersperse = defaultIntersperse
    find = defaultFind

instance IsSequence (Seq.Seq a) where
    fromList = Seq.fromList
    replicate = Seq.replicate
    replicateM = Seq.replicateM
    filter = Seq.filter
    --filterM = Seq.filterM
    break = Seq.breakl
    span = Seq.spanl
    dropWhile = Seq.dropWhileL
    takeWhile = Seq.takeWhileL
    splitAt = Seq.splitAt
    take = Seq.take
    drop = Seq.drop
    partition = Seq.partition
    uncons s =
        case Seq.viewl s of
            Seq.EmptyL -> Nothing
            x Seq.:< xs -> Just (x, xs)
    unsnoc s =
        case Seq.viewr s of
            Seq.EmptyR -> Nothing
            xs Seq.:> x -> Just (xs, x)
    --groupBy = Seq.groupBy

instance SemiSequence (V.Vector a) where
    type Index (V.Vector a) = Int
    singleton = V.singleton
    reverse = V.reverse
    find = V.find
    cons = V.cons
    snoc = V.snoc

    sortBy = defaultSortBy
    intersperse = defaultIntersperse

instance IsSequence (V.Vector a) where
    fromList = V.fromList
    replicate = V.replicate
    replicateM = V.replicateM
    filter = V.filter
    filterM = V.filterM
    break = V.break
    span = V.span
    dropWhile = V.dropWhile
    takeWhile = V.takeWhile
    splitAt = V.splitAt
    take = V.take
    drop = V.drop
    partition = V.partition
    uncons v
        | V.null v = Nothing
        | otherwise = Just (V.head v, V.tail v)
    unsnoc v
        | V.null v = Nothing
        | otherwise = Just (V.init v, V.last v)
    --groupBy = V.groupBy

instance U.Unbox a => SemiSequence (U.Vector a) where
    type Index (U.Vector a) = Int
    singleton = U.singleton

    intersperse = defaultIntersperse
    reverse = U.reverse
    find = U.find
    cons = U.cons
    snoc = U.snoc
    sortBy = defaultSortBy

instance U.Unbox a => IsSequence (U.Vector a) where
    fromList = U.fromList
    replicate = U.replicate
    replicateM = U.replicateM
    filter = U.filter
    filterM = U.filterM
    break = U.break
    span = U.span
    dropWhile = U.dropWhile
    takeWhile = U.takeWhile
    splitAt = U.splitAt
    take = U.take
    drop = U.drop
    partition = U.partition
    uncons v
        | U.null v = Nothing
        | otherwise = Just (U.head v, U.tail v)
    unsnoc v
        | U.null v = Nothing
        | otherwise = Just (U.init v, U.last v)
    --groupBy = U.groupBy

instance VS.Storable a => SemiSequence (VS.Vector a) where
    type Index (VS.Vector a) = Int
    singleton = VS.singleton
    reverse = VS.reverse
    find = VS.find
    cons = VS.cons
    snoc = VS.snoc

    intersperse = defaultIntersperse
    sortBy = defaultSortBy

instance VS.Storable a => IsSequence (VS.Vector a) where
    fromList = VS.fromList
    replicate = VS.replicate
    replicateM = VS.replicateM
    filter = VS.filter
    filterM = VS.filterM
    break = VS.break
    span = VS.span
    dropWhile = VS.dropWhile
    takeWhile = VS.takeWhile
    splitAt = VS.splitAt
    take = VS.take
    drop = VS.drop
    partition = VS.partition
    uncons v
        | VS.null v = Nothing
        | otherwise = Just (VS.head v, VS.tail v)
    unsnoc v
        | VS.null v = Nothing
        | otherwise = Just (VS.init v, VS.last v)
    --groupBy = U.groupBy

class (IsSequence seq, Eq (Element seq)) => EqSequence seq where
    stripPrefix :: seq -> seq -> Maybe seq
    stripPrefix x y = fmap fromList (otoList x `stripPrefix` otoList y)
    
    isPrefixOf :: seq -> seq -> Bool
    isPrefixOf x y = otoList x `isPrefixOf` otoList y
    
    stripSuffix :: seq -> seq -> Maybe seq
    stripSuffix x y = fmap fromList (otoList x `stripSuffix` otoList y)

    isSuffixOf :: seq -> seq -> Bool
    isSuffixOf x y = otoList x `isSuffixOf` otoList y

    isInfixOf :: seq -> seq -> Bool
    isInfixOf x y = otoList x `isInfixOf` otoList y

    group :: seq -> [seq]
    group = groupBy (==)
    
    -- | Similar to standard 'group', but operates on the whole collection, 
    -- not just the consecutive items.
    groupAll :: seq -> [seq]
    groupAll = groupAllOn id

    elem :: Element seq -> seq -> Bool
    elem e = List.elem e . otoList

    notElem :: Element seq -> seq -> Bool
    notElem e = List.notElem e . otoList

instance Eq a => EqSequence [a] where
    stripPrefix = List.stripPrefix
    isPrefixOf = List.isPrefixOf
    stripSuffix x y = fmap reverse (List.stripPrefix (reverse x) (reverse y))
    isSuffixOf x y = List.isPrefixOf (reverse x) (reverse y)
    isInfixOf = List.isInfixOf
    group = List.group
    elem = List.elem
    notElem = List.notElem

instance EqSequence S.ByteString where
    stripPrefix x y
        | x `S.isPrefixOf` y = Just (S.drop (S.length x) y)
        | otherwise = Nothing
    isPrefixOf = S.isPrefixOf
    stripSuffix x y
        | x `S.isSuffixOf` y = Just (S.take (S.length y - S.length x) y)
        | otherwise = Nothing
    isSuffixOf = S.isSuffixOf
    isInfixOf = S.isInfixOf
    group = S.group
    elem = S.elem
    notElem = S.notElem

instance EqSequence L.ByteString where
    stripPrefix x y
        | x `L.isPrefixOf` y = Just (L.drop (L.length x) y)
        | otherwise = Nothing
    isPrefixOf = L.isPrefixOf
    stripSuffix x y
        | x `L.isSuffixOf` y = Just (L.take (L.length y - L.length x) y)
        | otherwise = Nothing
    isSuffixOf = L.isSuffixOf
    isInfixOf x y = L.unpack x `List.isInfixOf` L.unpack y
    group = L.group
    elem = L.elem
    notElem = L.notElem

instance EqSequence T.Text where
    stripPrefix = T.stripPrefix
    isPrefixOf = T.isPrefixOf
    stripSuffix = T.stripSuffix
    isSuffixOf = T.isSuffixOf
    isInfixOf = T.isInfixOf
    group = T.group

instance EqSequence TL.Text where
    stripPrefix = TL.stripPrefix
    isPrefixOf = TL.isPrefixOf
    stripSuffix = TL.stripSuffix
    isSuffixOf = TL.isSuffixOf
    isInfixOf = TL.isInfixOf
    group = TL.group

instance Eq a => EqSequence (Seq.Seq a)
instance Eq a => EqSequence (V.Vector a)
instance (Eq a, U.Unbox a) => EqSequence (U.Vector a)
instance (Eq a, VS.Storable a) => EqSequence (VS.Vector a)

class (EqSequence seq, Ord (Element seq)) => OrdSequence seq where
    sort :: seq -> seq
    sort = fromList . List.sort . otoList

instance Ord a => OrdSequence [a] where
    sort = List.sort

instance OrdSequence S.ByteString where
    sort = S.sort

instance OrdSequence L.ByteString
instance OrdSequence T.Text
instance OrdSequence TL.Text
instance Ord a => OrdSequence (Seq.Seq a)
instance Ord a => OrdSequence (V.Vector a)
instance (Ord a, U.Unbox a) => OrdSequence (U.Vector a)
instance (Ord a, VS.Storable a) => OrdSequence (VS.Vector a)

class (IsSequence t, IsString t, Element t ~ Char) => Textual t where
    words :: t -> [t]
    unwords :: [t] -> t
    lines :: t -> [t]
    unlines :: [t] -> t
    toLower :: t -> t
    toUpper :: t -> t
    toCaseFold :: t -> t

    breakWord :: t -> (t, t)
    breakWord = fmap (dropWhile isSpace) . break isSpace

    breakLine :: t -> (t, t)
    breakLine =
        (killCR *** drop 1) . break (== '\n')
      where
        killCR t =
            case unsnoc t of
                Just (t', '\r') -> t'
                _ -> t

instance (c ~ Char) => Textual [c] where
    words = List.words
    unwords = List.unwords
    lines = List.lines
    unlines = List.unlines
    toLower = TL.unpack . TL.toLower . TL.pack
    toUpper = TL.unpack . TL.toUpper . TL.pack
    toCaseFold = TL.unpack . TL.toCaseFold . TL.pack

instance Textual T.Text where
    words = T.words
    unwords = T.unwords
    lines = T.lines
    unlines = T.unlines
    toLower = T.toLower
    toUpper = T.toUpper
    toCaseFold = T.toCaseFold

instance Textual TL.Text where
    words = TL.words
    unwords = TL.unwords
    lines = TL.lines
    unlines = TL.unlines
    toLower = TL.toLower
    toUpper = TL.toUpper
    toCaseFold = TL.toCaseFold

-- | A @map@-like function which doesn't obey the @Functor@ laws,
-- and/or requires extra constraints on the contained values.
class LooseMap t where
    type LooseMapConstraint t e :: Constraint
    looseMap :: (LooseMapConstraint t e1, LooseMapConstraint t e2) => (e1 -> e2) -> t e1 -> t e2
instance LooseMap Set.Set where
    type LooseMapConstraint Set.Set a = Ord a
    looseMap = Set.map
instance LooseMap HashSet.HashSet where
    type LooseMapConstraint HashSet.HashSet a = (Eq a, Hashable a)
    looseMap = HashSet.map
instance LooseMap U.Vector where
    type LooseMapConstraint U.Vector a = U.Unbox a
    looseMap = U.map
instance LooseMap VS.Vector where
    type LooseMapConstraint VS.Vector a = VS.Storable a
    looseMap = VS.map
