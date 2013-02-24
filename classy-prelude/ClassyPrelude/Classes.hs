{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
module ClassyPrelude.Classes where

import CorePrelude
import qualified Data.List as List


class CanMap ci co i o | ci -> i, co -> o, ci o -> co, co i -> ci where
    map :: (i -> o) -> ci -> co

class CanConcatMap ci co i o | ci -> i, co -> o, ci o -> co, co i -> ci where
    concatMap :: (i -> o) -> ci -> co

class CanFilter c i | c -> i where
    filter :: (i -> Bool) -> c -> c

class CanFilterM c i | c -> i where
    filterM :: Monad m => (i -> m Bool) -> c -> m c

class CanLength c len | c -> len where
    length :: c -> len

class CanSingleton c i | c -> i where
    singleton :: i -> c

class CanNull c where
    null :: c -> Bool

class CanPack c i | c -> i where
    pack :: [i] -> c
    unpack :: c -> [i]
    subsequences :: c -> [c]
    subsequences = List.map pack . List.subsequences . unpack
    permutations :: c -> [c]
    permutations = List.map pack . List.permutations . unpack

class CanIntersperse c i | c -> i where
    intersperse :: i -> c -> c

class Monad m => CanMapM ci mco m i o | ci -> i, mco -> m o, ci o m -> mco, mco i -> ci where
    mapM :: (i -> m o) -> ci -> mco

class CanMapM_ ci i | ci -> i where
    mapM_ :: Monad m => (i -> m o) -> ci -> m ()

class CanReplicateM c i len | c -> i len where
    replicateM :: Monad m => len -> m i -> m c

class CanLookup c k v | c -> k v where
    lookup :: k -> c -> Maybe v

class CanInsert f where
    insert :: f

class CanInsertVal c k v | c -> k v where
    insertVal :: k -> v -> c -> c

instance (CanInsertVal c' k v, c ~ c') => CanInsert (k -> v -> c -> c') where
    insert = insertVal

class CanDelete f where
    delete :: f

class CanDeleteVal c k | c -> k where
    deleteVal :: k -> c -> c

instance (CanDeleteVal c' k, c ~ c') => CanDelete (k -> c -> c') where
    delete = deleteVal

class CanMember c k | c -> k where
    member :: k -> c -> Bool
    notMember :: k -> c -> Bool
    notMember k = not . member k

class CanReadFile a where
    readFile :: MonadIO m => FilePath -> m a

class CanWriteFile a where
    writeFile :: MonadIO m => FilePath -> a -> m ()

class CanStripPrefix a where
    stripPrefix :: a -> a -> Maybe a
    isPrefixOf :: a -> a -> Bool

class CanBreak c i | c -> i where
    break :: (i -> Bool) -> c -> (c, c)
    span :: (i -> Bool) -> c -> (c, c)
    dropWhile :: (i -> Bool) -> c -> c
    takeWhile :: (i -> Bool) -> c -> c

class CanAny c i | c -> i where
    any :: (i -> Bool) -> c -> Bool
    all :: (i -> Bool) -> c -> Bool

class CanSplitAt c i | c -> i where
    splitAt :: i -> c -> (c, c)

class CanFold c i accum | c -> i where
    -- | Strict left fold.
    fold :: (accum -> i -> accum) -> accum -> c -> accum

class CanWords t where
    words :: t -> [t]
    unwords :: [t] -> t

class CanLines t where
    lines :: t -> [t]

class CanUnlines t where
    unlines :: [t] -> t

class CanSplit c i | c -> i where
    split :: (i -> Bool) -> c -> [c]

class CanStripSuffix a where
    stripSuffix :: a -> a -> Maybe a
    isSuffixOf :: a -> a -> Bool

class CanIsInfixOf a where
    isInfixOf :: a -> a -> Bool

class CanReverse a where
    reverse :: a -> a

class CanReplicate a i len | a -> i len where
    replicate :: len -> i -> a

class CanToChunks c i | c -> i, i -> c where
    toChunks :: c -> [i]
    fromChunks :: [i] -> c

class CanEncodeUtf8 ci co | co -> ci, ci -> co where
    encodeUtf8 :: ci -> co

-- | Note: implementations should ensure that @decodeUtf8@ is a total
-- function. As such, the standard @decodeUtf8@ provided by the text package
-- should not be used, but instead @decodeUtf8With lenientDecode@.
class CanDecodeUtf8 ci co | co -> ci, ci -> co where
    decodeUtf8 :: ci -> co

class CanToStrict a b where
    toStrict :: a -> b
    fromStrict :: b -> a

class CanGetLine a where
    getLine :: a

class CanToLower a where
    toLower :: a -> a

class CanToUpper a where
    toUpper :: a -> a

class CanToCaseFold a where
    toCaseFold :: a -> a

class CanFind c i | c -> i where
    find :: (i -> Bool) -> c -> Maybe i

class CanConcat c i | c -> i where
    concat :: c -> i

class CanPartition c i | c -> i where
    partition :: (i -> Bool) -> c -> (c, c)

class CanNubBy c i | c -> i where
    nubBy :: (i -> i -> Bool) -> c -> c

    nub :: (Ord i, CanNubBy c i) => c -> c
    nub = nubBy (==)

class CanUnion c where
    union :: c -> c -> c

class CanDifference c where
    difference :: c -> c -> c

class CanIntersection c where
    intersection :: c -> c -> c

class CanSortBy c a | c -> a where
    sortBy :: (a -> a -> Ordering) -> c -> c

class Ord a => CanSort c a | c -> a where
    sort :: c -> c
    default sort :: CanSortBy c a => c -> c
    sort = sortBy compare

class CanCons c a where
    cons :: a -> c -> c

class CanUncons c a where
    uncons :: c -> Maybe (a, c)

class CanCompareLength c where
    -- | This is a more effective alternative to statements like @i >= length 
    -- xs@ for types having an O(n) complexity of `length` operation like list 
    -- or `Text`. It does not traverse the whole data structure if the value
    -- being compared to is lesser.
    compareLength :: (Integral l) => c -> l -> Ordering
