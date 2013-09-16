{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Sequences where

import Data.Monoid
import Data.MonoTraversable
import Data.Int (Int64, Int)
import qualified Data.List as List
import qualified Control.Monad (filterM, replicateM)
import Prelude (Bool (..), Monad (..), Maybe (..), Ordering (..), Ord (..), Eq (..), Functor (..), fromIntegral, otherwise, (-), not, fst, snd, Integral)
import Data.Char (Char)
import Data.Word (Word8)
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
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import Data.Text.Encoding.Error (lenientDecode)

-- | Laws:
--
-- > fromList . toList = id
-- > fromList (x <> y) = fromList x <> fromList y
-- > otoList (fromList x <> fromList y) = x <> y
class ( Monoid container
      , MonoTraversable container
      , Integral (Index container)
      ) => IsSequence container
  where
    type Index container
    singleton :: Element container -> container

    fromList :: [Element container] -> container
    fromList = mconcat . fmap singleton

    replicate :: Index container -> Element container -> container
    replicate i = fromList . List.genericReplicate i

    replicateM :: Monad m
               => Index container
               -> m (Element container)
               -> m container
    replicateM i = liftM fromList . Control.Monad.replicateM (fromIntegral i)

    filter :: (Element container -> Bool) -> container -> container
    filter f = fromList . List.filter f . otoList

    filterM :: Monad m
            => (Element container -> m Bool)
            -> container
            -> m container
    filterM f = Control.Monad.liftM fromList . filterM f . otoList

    intersperse :: Element container -> container -> container
    intersperse e = fromList . List.intersperse e . otoList

    break :: (Element container -> Bool) -> container -> (container, container)
    break f = (fromList *** fromList) . List.break f . otoList

    span :: (Element container -> Bool) -> container -> (container, container)
    span f = (fromList *** fromList) . List.span f . otoList

    dropWhile :: (Element container -> Bool) -> container -> container
    dropWhile f = fromList . List.dropWhile f . otoList
    
    takeWhile :: (Element container -> Bool) -> container -> container
    takeWhile f = fromList . List.takeWhile f . otoList

    splitAt :: Index container -> container -> (container, container)
    splitAt i = (fromList *** fromList) . List.genericSplitAt i . otoList

    take :: Index container -> container -> container
    take i = fst . splitAt i

    drop :: Index container -> container -> container
    drop i = snd . splitAt i

    -- FIXME split :: (Element c -> Bool) -> c -> [c]

    reverse :: container -> container
    reverse = fromList . List.reverse . otoList

    find :: (Element container -> Bool)
         -> container
         -> Maybe (Element container)
    find f = List.find f . otoList
    
    partition :: (Element container -> Bool)
              -> container
              -> (container, container)
    partition f = (fromList *** fromList) . List.partition f . otoList
    
    sortBy :: (Element container -> Element container -> Ordering)
           -> container
           -> container
    sortBy f = fromList . List.sortBy f . otoList
    
    cons :: Element container -> container -> container
    cons e = fromList . (e:) . otoList

    uncons :: container -> Maybe (Element container, container)
    uncons = fmap (second fromList) . uncons . otoList

    groupBy :: (Element container -> Element container -> Bool)
            -> container
            -> [container]
    groupBy f = fmap fromList . List.groupBy f . otoList

    -- | Similar to standard 'groupBy', but operates on the whole collection, 
    -- not just the consecutive items.
    groupAllBy :: (Element container -> Element container -> Bool)
               -> container
               -> [container]
    groupAllBy f = fmap fromList . groupAllBy f . otoList

    subsequences :: container -> [container]
    subsequences = List.map fromList . List.subsequences . otoList

    permutations :: container -> [container]
    permutations = List.map fromList . List.permutations . otoList

instance IsSequence [a] where
    type Index [a] = Int
    singleton = return
    fromList = id
    {-# INLINE fromList #-}
    replicate = List.replicate
    replicateM = Control.Monad.replicateM
    filter = List.filter
    filterM = Control.Monad.filterM
    intersperse = List.intersperse
    break = List.break
    span = List.span
    dropWhile = List.dropWhile
    takeWhile = List.takeWhile
    splitAt = List.splitAt
    take = List.take
    drop = List.drop
    reverse = List.reverse
    find = List.find
    partition = List.partition
    sortBy = List.sortBy
    cons = (:)
    uncons [] = Nothing
    uncons (x:xs) = Just (x, xs)
    groupBy = List.groupBy
    groupAllBy f (head : tail) =
        (head : matches) : groupAllBy f nonMatches
      where
        (matches, nonMatches) = partition (f head) tail
    groupAllBy _ [] = []

instance IsSequence S.ByteString where
    type Index S.ByteString = Int
    singleton = S.singleton
    fromList = S.pack
    replicate = S.replicate
    filter = S.filter
    intersperse = S.intersperse
    break = S.break
    span = S.span
    dropWhile = S.dropWhile
    takeWhile = S.takeWhile
    splitAt = S.splitAt
    take = S.take
    drop = S.drop
    reverse = S.reverse
    find = S.find
    partition = S.partition
    cons = S.cons
    uncons = S.uncons
    groupBy = S.groupBy
    -- sortBy

instance IsSequence T.Text where
    type Index T.Text = Int
    singleton = T.singleton
    fromList = T.pack
    replicate i c = T.replicate i (T.singleton c)
    filter = T.filter
    intersperse = T.intersperse
    break = T.break
    span = T.span
    dropWhile = T.dropWhile
    takeWhile = T.takeWhile
    splitAt = T.splitAt
    take = T.take
    drop = T.drop
    reverse = T.reverse
    find = T.find
    partition = T.partition
    cons = T.cons
    uncons = T.uncons
    groupBy = T.groupBy
    -- sortBy

instance IsSequence L.ByteString where
    type Index L.ByteString = Int64
    singleton = L.singleton
    fromList = L.pack
    replicate = L.replicate
    filter = L.filter
    intersperse = L.intersperse
    break = L.break
    span = L.span
    dropWhile = L.dropWhile
    takeWhile = L.takeWhile
    splitAt = L.splitAt
    take = L.take
    drop = L.drop
    reverse = L.reverse
    find = L.find
    partition = L.partition
    cons = L.cons
    uncons = L.uncons
    groupBy = L.groupBy
    -- sortBy

instance IsSequence TL.Text where
    type Index TL.Text = Int64
    singleton = TL.singleton
    fromList = TL.pack
    replicate i c = TL.replicate i (TL.singleton c)
    filter = TL.filter
    intersperse = TL.intersperse
    break = TL.break
    span = TL.span
    dropWhile = TL.dropWhile
    takeWhile = TL.takeWhile
    splitAt = TL.splitAt
    take = TL.take
    drop = TL.drop
    reverse = TL.reverse
    find = TL.find
    partition = TL.partition
    cons = TL.cons
    uncons = TL.uncons
    groupBy = TL.groupBy
    -- sortBy


instance IsSequence (Seq.Seq a) where
    type Index (Seq.Seq a) = Int
    singleton = Seq.singleton
    fromList = Seq.fromList
    replicate = Seq.replicate
    replicateM = Seq.replicateM
    filter = Seq.filter
    --filterM = Seq.filterM
    --intersperse = Seq.intersperse
    break = Seq.breakl
    span = Seq.spanl
    dropWhile = Seq.dropWhileL
    takeWhile = Seq.takeWhileL
    splitAt = Seq.splitAt
    take = Seq.take
    drop = Seq.drop
    reverse = Seq.reverse
    --find = Seq.find
    partition = Seq.partition
    sortBy = Seq.sortBy
    cons = (Seq.<|)
    uncons s =
        case Seq.viewl s of
            Seq.EmptyL -> Nothing
            x Seq.:< xs -> Just (x, xs)
    --groupBy = Seq.groupBy

instance IsSequence (V.Vector a) where
    type Index (V.Vector a) = Int
    singleton = V.singleton
    fromList = V.fromList
    replicate = V.replicate
    replicateM = V.replicateM
    filter = V.filter
    filterM = V.filterM
    --intersperse = V.intersperse
    break = V.break
    span = V.span
    dropWhile = V.dropWhile
    takeWhile = V.takeWhile
    splitAt = V.splitAt
    take = V.take
    drop = V.drop
    reverse = V.reverse
    find = V.find
    partition = V.partition
    --sortBy = V.sortBy
    cons = V.cons
    uncons v
        | V.null v = Nothing
        | otherwise = Just (V.head v, V.tail v)
    --groupBy = V.groupBy

instance U.Unbox a => IsSequence (U.Vector a) where
    type Index (U.Vector a) = Int
    singleton = U.singleton
    fromList = U.fromList
    replicate = U.replicate
    replicateM = U.replicateM
    filter = U.filter
    filterM = U.filterM
    --intersperse = U.intersperse
    break = U.break
    span = U.span
    dropWhile = U.dropWhile
    takeWhile = U.takeWhile
    splitAt = U.splitAt
    take = U.take
    drop = U.drop
    reverse = U.reverse
    find = U.find
    partition = U.partition
    --sortBy = U.sortBy
    cons = U.cons
    uncons v
        | U.null v = Nothing
        | otherwise = Just (U.head v, U.tail v)
    --groupBy = U.groupBy

class ( IsSequence container
      , Eq (Element container)
      ) => EqSequence container
  where
    stripPrefix :: container -> container -> Maybe container
    stripPrefix x y = fmap fromList (otoList x `stripPrefix` otoList y)
    
    isPrefixOf :: container -> container -> Bool
    isPrefixOf x y = otoList x `isPrefixOf` otoList y
    
    stripSuffix :: container -> container -> Maybe container
    stripSuffix x y = fmap fromList (otoList x `stripSuffix` otoList y)

    isSuffixOf :: container -> container -> Bool
    isSuffixOf x y = otoList x `isSuffixOf` otoList y

    isInfixOf :: container -> container -> Bool
    isInfixOf x y = otoList x `isInfixOf` otoList y

    group :: container -> [container]
    group = groupBy (==)
    
    -- | Similar to standard 'group', but operates on the whole collection, 
    -- not just the consecutive items.
    groupAll :: container -> [container]
    groupAll = groupAllBy (==)

    elem :: Element container -> container -> Bool
    elem e = List.elem e . otoList

    notElem :: Element container -> container -> Bool
    notElem e = List.notElem e . otoList

instance Eq a => EqSequence [a] where
    stripPrefix = List.stripPrefix
    isPrefixOf = List.isPrefixOf
    stripSuffix x y = fmap reverse (List.stripPrefix (reverse x) (reverse y))
    isSuffixOf x y = reverse x `List.isPrefixOf` reverse y
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

class ( EqSequence container
      , Ord (Element container)
      ) => OrdSequence container
  where
    sort :: container -> container
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

class ( IsSequence lazy
      , IsSequence strict
      ) => LazySequence lazy strict | lazy -> strict, strict -> lazy
  where
    toChunks :: lazy -> [strict]
    fromChunks :: [strict] -> lazy
    toStrict :: lazy -> strict
    fromStrict :: strict -> lazy

instance LazySequence L.ByteString S.ByteString where
    toChunks = L.toChunks
    fromChunks = L.fromChunks
    toStrict = mconcat . L.toChunks
    fromStrict = L.fromChunks . return

instance LazySequence TL.Text T.Text where
    toChunks = TL.toChunks
    fromChunks = TL.fromChunks
    toStrict = TL.toStrict
    fromStrict = TL.fromStrict

class ( IsSequence text
      , IsSequence bin
      ) => Textual text bin | text -> bin, bin -> text
  where
    words :: text -> [text]
    unwords :: [text] -> text
    lines :: text -> [text]
    unlines :: [text] -> text
    encodeUtf8 :: text -> bin
    decodeUtf8 :: bin -> text
    toLower :: text -> text
    toUpper :: text -> text
    toCaseFold :: text -> text

instance (char ~ Char, word8 ~ Word8) => Textual [char] [word8] where
    words = List.words
    unwords = List.unwords
    lines = List.lines
    unlines = List.unlines
    encodeUtf8 = L.unpack . TL.encodeUtf8 . TL.pack
    decodeUtf8 = TL.unpack . TL.decodeUtf8With lenientDecode . L.pack
    toLower = TL.unpack . TL.toLower . TL.pack
    toUpper = TL.unpack . TL.toUpper . TL.pack
    toCaseFold = TL.unpack . TL.toCaseFold . TL.pack

instance Textual T.Text S.ByteString where
    words = T.words
    unwords = T.unwords
    lines = T.lines
    unlines = T.unlines
    encodeUtf8 = T.encodeUtf8
    decodeUtf8 = T.decodeUtf8With lenientDecode
    toLower = T.toLower
    toUpper = T.toUpper
    toCaseFold = T.toCaseFold

instance Textual TL.Text L.ByteString where
    words = TL.words
    unwords = TL.unwords
    lines = TL.lines
    unlines = TL.unlines
    encodeUtf8 = TL.encodeUtf8
    decodeUtf8 = TL.decodeUtf8With lenientDecode
    toLower = TL.toLower
    toUpper = TL.toUpper
    toCaseFold = TL.toCaseFold
