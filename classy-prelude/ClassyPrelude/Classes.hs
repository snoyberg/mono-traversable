{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
module ClassyPrelude.Classes where

import Prelude ()
import CorePrelude
import qualified Data.List as List


class CanMap f i o where
    map :: (i -> o) -> f

class CanMapFunc ci co i o | ci -> i, co -> o, ci o -> co, co i -> ci where
    mapFunc :: (i -> o) -> ci -> co

instance CanMapFunc ci co i o => CanMap (ci -> co) i o where
    map = mapFunc

class CanConcatMap f i o where
    concatMap :: (i -> o) -> f

class CanConcatMapFunc ci co i o | ci -> i, co -> o, ci o -> co, co i -> ci where
    concatMapFunc :: (i -> o) -> ci -> co

instance CanConcatMapFunc ci co i o => CanConcatMap (ci -> co) i o where
    concatMap = concatMapFunc

class CanFilter f i where
    filter :: (i -> Bool) -> f

class CanFilterFunc ci co i | ci -> i co where
    filterFunc :: (i -> Bool) -> ci -> co

instance (CanFilterFunc ci co i, ci ~ co) => CanFilter (ci -> co) i where
    filter = filterFunc

class CanFilterM f m i where
    filterM :: (i -> m Bool) -> f

class CanFilterMFunc c i | c -> i where
    filterMFunc :: Monad m => (i -> m Bool) -> c -> m c

instance (CanFilterMFunc ci i, m ci ~ mco, Monad m) => CanFilterM (ci -> mco) m i where
    filterM = filterMFunc

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

class Monad m => CanMapM f m i o where
    mapM :: (i -> m o) -> f

class Monad m => CanMapMFunc ci mco m i o | ci -> i, mco -> m o, ci o m -> mco, mco i -> ci where
    mapMFunc :: (i -> m o) -> ci -> mco

instance CanMapMFunc ci mco m i o => CanMapM (ci -> mco) m i o where
    mapM = mapMFunc

class Monad m => CanMapM_ f m i where
    mapM_ :: (i -> m o) -> f

class CanMapM_Func ci i | ci -> i where
    mapM_Func :: Monad m => (i -> m o) -> ci -> m ()

instance (Monad m, CanMapM_Func ci i, r ~ m ()) => CanMapM_ (ci -> r) m i where
    mapM_ = mapM_Func

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
    readFile :: FilePath -> a

class CanWriteFile a where
    writeFile :: FilePath -> a

class CanWriteFileFunc a where
    writeFileFunc :: MonadIO m => FilePath -> a -> m ()

instance (MonadIO m, b ~ (), CanWriteFileFunc a) => CanWriteFile (a -> m b) where
    writeFile = writeFileFunc

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

class CanFold f i accum where
    -- | Strict left fold.
    fold :: (accum -> i -> accum) -> accum -> f

class CanFoldFunc c i accum | c -> i where
    -- | Strict left fold.
    foldFunc :: (accum -> i -> accum) -> accum -> c -> accum

instance (CanFoldFunc c i accum, accum ~ result) => CanFold (c -> result) i accum where
    fold = foldFunc

class CanWords t where
    words :: t -> [t]
    unwords :: [t] -> t

class CanLines f where
    lines :: f

class CanLinesFunc t where
    linesFunc :: t -> [t]

instance (CanLinesFunc t, out ~ [t]) => CanLines (t -> out) where
    lines = linesFunc

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

class CanEncodeUtf8 f where
    encodeUtf8 :: f

class CanEncodeUtf8Func ci co | co -> ci, ci -> co where
    encodeUtf8Func :: ci -> co

instance CanEncodeUtf8Func ci co => CanEncodeUtf8 (ci -> co) where
    encodeUtf8 = encodeUtf8Func

class CanDecodeUtf8 f where
    decodeUtf8 :: f
-- | Note: implementations should ensure that @decodeUtf8Func@ is a total
-- function. As such, the standard @decodeUtf8@ provided by the text package
-- should not be used, but instead @decodeUtf8With lenientDecode@.

class CanDecodeUtf8Func ci co | co -> ci, ci -> co where
    decodeUtf8Func :: ci -> co

instance CanDecodeUtf8Func ci co => CanDecodeUtf8 (ci -> co) where
    decodeUtf8 = decodeUtf8Func

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

class CanSortBy c a where
    sortBy :: (a -> a -> Ordering) -> c a -> c a
    sort :: c a -> c a
    default sort :: (Ord a) => c a -> c a
    sort = sortBy compare
