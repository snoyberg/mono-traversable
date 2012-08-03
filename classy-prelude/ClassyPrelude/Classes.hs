{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.Classes where

import qualified Prelude
import Prelude (Monad)
import qualified Filesystem.Path.CurrentOS as F
import Control.Monad.IO.Class (MonadIO)

class CanMap ci co i o where
    map :: (i -> o) -> ci -> co

class CanConcatMap ci co i o where
    concatMap :: (i -> o) -> ci -> co

class CanFilter c i | c -> i where
    filter :: (i -> Prelude.Bool) -> c -> c

class CanLength c len | c -> len where
    length :: c -> len

class CanSingleton c i | c -> i where
    singleton :: i -> c

class CanNull c where
    null :: c -> Prelude.Bool

class CanPack c i | c -> i where
    pack :: [i] -> c
    unpack :: c -> [i]

class CanMapM ci co i o where
    mapM :: Monad m => (i -> m o) -> ci -> m co

class CanMapM_ ci i where
    mapM_ :: Monad m => (i -> m o) -> ci -> m ()

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

take :: CanSplitAt c i => i -> c -> c
take i c  = Prelude.fst (splitAt i c)
drop :: CanSplitAt c i => i -> c -> c
drop i c  = Prelude.snd (splitAt i c)

class CanFold c i accum | c -> i where
    -- | Strict left fold.
    fold :: (accum -> i -> accum) -> accum -> c -> accum
