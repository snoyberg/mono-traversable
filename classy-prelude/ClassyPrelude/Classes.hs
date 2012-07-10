{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.Classes where

import qualified Prelude
import qualified Filesystem.Path.CurrentOS as F
import Control.Monad.IO.Class (MonadIO)

class CanMap f i o where
    map :: (i -> o) -> f
class CanMapFunc ci co i o where
    mapFunc :: (i -> o) -> ci -> co
instance CanMapFunc ci co i o => CanMap (ci -> co) i o where
    map = mapFunc

class CanConcatMap f i o where
    concatMap :: (i -> o) -> f
class CanConcatMapFunc ci co i o where
    concatMapFunc :: (i -> o) -> ci -> co
instance CanConcatMapFunc ci co i o => CanConcatMap (ci -> co) i o where
    concatMap = concatMapFunc

class CanFilter f a where
    filter :: (a -> Prelude.Bool) -> f
class CanFilterFunc c i | c -> i where
    filterFunc :: (i -> Prelude.Bool) -> c -> c
instance (b ~ c, CanFilterFunc b a) => CanFilter (b -> c) a where
    filter = filterFunc

class CanLength c i | c -> i where
    length :: c -> i

class CanSingleton c i | c -> i where
    singleton :: i -> c

class CanNull c where
    null :: c -> Prelude.Bool

class CanPack c i | c -> i where
    pack :: [i] -> c
    unpack :: c -> [i]

class CanMapM f i o m where
    mapM :: (i -> m o) -> f
class CanMapMFunc ci co i o m where
    mapMFunc :: (i -> m o) -> ci -> m co
instance CanMapMFunc ci co i o m => CanMapM (ci -> m co) i o m where
    mapM = mapMFunc

class CanMapM_ f i o m where
    mapM_ :: (i -> m o) -> f
class CanMapM_Func ci i o m where
    mapM_Func :: (i -> m o) -> ci -> m ()
instance (x ~ (), CanMapM_Func ci i o m) => CanMapM_ (ci -> m x) i o m where
    mapM_ = mapM_Func

class CanLookup c k v | c -> k v where
    lookup :: k -> c -> Prelude.Maybe v

class CanEmpty c where
    empty :: c

class CanInsert f where
    insert :: f
class CanInsertVal c k v | c -> k v where
    insertVal :: k -> v -> c -> c
instance (CanInsertVal c' k v, c ~ c') => CanInsert (k -> v -> c -> c') where
    insert = insertVal

class CanDelete c k | c -> k where
    delete :: k -> c -> c

class CanMember c k | c -> k where
    member :: k -> c -> Prelude.Bool

class CanReadFile a where
    readFile :: F.FilePath -> a

class CanWriteFile a where
    writeFile :: F.FilePath -> a
class CanWriteFileFunc a where
    writeFileFunc :: MonadIO m => F.FilePath -> a -> m ()
instance (MonadIO m, b ~ (), CanWriteFileFunc a) => CanWriteFile (a -> m b) where
    writeFile = writeFileFunc

class CanStripPrefix a where
    stripPrefix :: a -> a -> Prelude.Maybe a

class CanBreak c i | c -> i where
    break :: (i -> Prelude.Bool) -> c -> (c, c)
    span :: (i -> Prelude.Bool) -> c -> (c, c)
    dropWhile :: (i -> Prelude.Bool) -> c -> c
    takeWhile :: (i -> Prelude.Bool) -> c -> c

class CanAny c i | c -> i where
    any :: (i -> Prelude.Bool) -> c -> Prelude.Bool
    all :: (i -> Prelude.Bool) -> c -> Prelude.Bool

class CanSplitAt c i | c -> i where
    splitAt :: i -> c -> (c, c)

class CanFold accum a f where
    -- | Strict left fold.
    fold :: (accum -> a -> accum) -> accum -> f
class CanFoldFunc c accum a | c -> a where
    foldFunc :: (accum -> a -> accum) -> accum -> c -> accum
instance (accum ~ accum', CanFoldFunc c accum a) => CanFold accum a (c -> accum') where
    fold = foldFunc
