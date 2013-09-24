{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module ClassyPrelude.Conduit
    ( -- * Re-export
      module ClassyPrelude
    , module ClassyPrelude.Conduit
    , module Data.Conduit
    , module Data.Conduit.List
    ) where

import ClassyPrelude

import Data.Conduit
import Data.Conduit.List (consume, sinkNull)
import qualified System.IO as SIO
import qualified Data.Conduit.List as CL
import qualified Filesystem as F

stdinLnC :: (MonadIO m, IOData a) => Producer m a
stdinLnC = fromHandleC stdin

readLnC :: (MonadIO m, Read a) => Producer m (Vector a)
readLnC = stdinLnC =$= readC

fromHandleC :: (MonadIO m, IOData a) => Handle -> Producer m a
fromHandleC h =
    loop
  where
    loop = do
        eof <- liftIO $ SIO.hIsEOF h
        unless eof $ do
            hGetLine h >>= yield
            loop

replicateMC :: Monad m => Int -> m a -> Producer m (Vector a)
replicateMC i m = replicateM_ i (lift m >>= yield . singleton)

stdoutLnC :: (MonadIO m, IOData a) => Consumer a m ()
stdoutLnC = toHandleC stdout

printC :: (MonadIO m, Show a) => Consumer (Vector a) m ()
printC = mapM_C print

toHandleC :: (MonadIO m, IOData a) => Handle -> Consumer a m ()
toHandleC h = awaitForever $ hPutStrLn h

mapC :: (Functor f, Monad m) => (a -> b) -> Conduit (f a) m (f b)
mapC = CL.map . fmap

omapC :: (MonoFunctor c, Monad m) => (Element c -> Element c) -> Conduit c m c
omapC = CL.map . omap

mapMC :: (Traversable t, Monad m) => (a -> m b) -> Conduit (t a) m (t b)
mapMC = CL.mapM . mapM

omapMC :: (MonoTraversable c, Monad m) => (Element c -> m (Element c)) -> Conduit c m c
omapMC = CL.mapM . omapM

mapM_C :: (MonoFoldable c, Monad m) => (Element c -> m ()) -> Consumer c m ()
mapM_C = CL.mapM_ . mapM_

filterC :: (IsSequence c, Monad m) => (Element c -> Bool) -> Conduit c m c
filterC = CL.map . filter

filterMC :: (IsSequence c, Monad m) => (Element c -> m Bool) -> Conduit c m c
filterMC = CL.mapM . filterM

takeC :: (IsSequence c, Monad m) => Index c -> Conduit c m c
takeC =
    loop
  where
    loop remaining =
        await >>= maybe (return ()) go
      where
        go c =
            case compare remaining l of
                LT ->
                    let (x, y) = splitAt remaining c
                     in yield x >> leftover y
                EQ -> yield c
                GT -> yield c >> loop (remaining - l)
          where
            l = fromIntegral (length c)

takeWhileC :: (IsSequence c, Monad m) => (Element c -> Bool) -> Conduit c m c
takeWhileC f =
    loop
  where
    loop =
        await >>= maybe (return ()) go

    go c
        | null y = yield x >> loop
        | otherwise = yield x >> leftover y
      where
        (x, y) = span f c

dropC :: (IsSequence c, Monad m) => Index c -> Consumer c m ()
dropC =
    loop
  where
    loop remaining =
        await >>= maybe (return ()) go
      where
        go c =
            case compare remaining l of
                LT -> leftover $ drop remaining c
                EQ -> return ()
                GT -> loop (remaining - l)
          where
            l = fromIntegral (length c)

dropWhileC :: (IsSequence c, Monad m) => (Element c -> Bool) -> Conduit c m c
dropWhileC f =
    loop
  where
    loop =
        await >>= maybe (return ()) go

    go c
        | null y = loop
        | otherwise = leftover y
      where
        y = dropWhile f c

concatC :: (Monad m, MonoFoldable c) => Conduit c m (Element c)
concatC = awaitForever $ mapM_ yield

unconcatC :: (Monad m, IsSequence c) => Conduit (Element c) m c
unconcatC = awaitForever $ yield . singleton

elemIndicesC :: (MonoFoldable c, Eq (Element c), Monad m) => Element c -> Conduit c m Int
elemIndicesC x = findIndicesC (== x)

findIndicesC :: (MonoFoldable c, Monad m) => (Element c -> Bool) -> Conduit c m Int
findIndicesC f =
    loop 0
  where
    loop i =
        await >>= maybe (return ()) (foldM maybeYield i >=> loop)

    maybeYield i x = do
        when (f x) $ yield i
        return $! i + 1

chainC :: (MonoFoldable c, Monad m) => (Element c -> m ()) -> Conduit c m c
chainC f = awaitForever $ \c -> do
    lift $ mapM_ f c
    yield c

readC :: (Monad m, Read (Element c), IsSequence c) => Conduit String m c
readC = awaitForever (maybe (return ()) (yield . singleton) . readMay)

showC :: (Monad m, Show (Element c), MonoFoldable c) => Conduit c m String
showC = awaitForever $ mapM_ (yield . show)

foldC :: (Monad m, MonoFoldable c) => (a -> Element c -> a) -> a -> Consumer c m a
foldC f = CL.fold (foldl' f)

foldMC :: (Monad m, MonoFoldable c) => (a -> Element c -> m a) -> a -> Consumer c m a
foldMC f = CL.foldM (foldM f)

allC :: (Monad m, MonoFoldable c) => (Element c -> Bool) -> Consumer c m Bool
allC f =
    loop
  where
    loop = await >>= maybe (return True) go
    go c
        | all f c = loop
        | otherwise = return False

anyC :: (Monad m, MonoFoldable c) => (Element c -> Bool) -> Consumer c m Bool
anyC f =
    loop
  where
    loop = await >>= maybe (return False) go
    go c
        | any f c = return True
        | otherwise = loop

andC :: (Monad m, MonoFoldable c, Element c ~ Bool) => Consumer c m Bool
andC = allC id

orC :: (Monad m, MonoFoldable c, Element c ~ Bool) => Consumer c m Bool
orC = anyC id

elemC :: (Monad m, EqSequence c) => Element c -> Consumer c m Bool
elemC x =
    loop
  where
    loop = await >>= maybe (return False) go
    go c
        | x `elem` c = return True
        | otherwise = loop

notElemC :: (Monad m, EqSequence c) => Element c -> Consumer c m Bool
notElemC = fmap not . elemC

findC :: (Monad m, IsSequence c) => (Element c -> Bool) -> Consumer c m (Maybe (Element c))
findC f =
    loop
  where
    loop = await >>= maybe (return Nothing) go
    go c =
        case find f c of
            Just x -> return (Just x)
            Nothing -> loop

findIndexC :: (Monad m, IsSequence c) => (Element c -> Bool) -> Consumer c m (Maybe Int)
findIndexC f =
    loop 0
  where
    loop i = await >>= maybe (return Nothing) (go i . toList)

    go i [] = loop i
    go i (x:xs)
        | f x = return (Just i)
        | otherwise =
            let j = i + 1
             in j `seq` go j xs

headC :: (Monad m, IsSequence c) => Consumer c m (Maybe (Element c))
headC =
    loop
  where
    loop = await >>= maybe (return Nothing) go
    go c =
        case uncons c of
            Nothing -> loop
            Just (x, c') -> leftover c' >> return (Just x)

peekC :: (Monad m, IsSequence c) => Consumer c m (Maybe (Element c))
peekC =
    loop
  where
    loop = await >>= maybe (return Nothing) go
    go c =
        case uncons c of
            Nothing -> loop
            Just (x, _) -> leftover c >> return (Just x)

lengthC :: (Monad m, MonoFoldable c) => Consumer c m Int
lengthC = CL.fold (\i x -> i + length x) 0

nullC :: (Monad m, MonoFoldable c) => Consumer c m Bool
nullC =
    loop
  where
    loop = await >>= maybe (return True) go
    go c
        | null c = loop
        | otherwise = leftover c >> return False

sumC :: (Monad m, MonoFoldable c, Num (Element c)) => Consumer c m (Element c)
sumC = foldC (+) 0

productC :: (Monad m, MonoFoldable c, Num (Element c)) => Consumer c m (Element c)
productC = foldC (*) 1

sourceFile :: (MonadResource m, IOData a) => FilePath -> Producer m a
sourceFile fp = sourceIOHandle (F.openFile fp SIO.ReadMode)

sourceHandle :: (MonadIO m, IOData a) => Handle -> Producer m a
sourceHandle h =
    loop
  where
    loop = do
        x <- liftIO (hGetChunk h)
        if null x
            then return ()
            else yield x >> loop

sourceIOHandle :: (MonadResource m, IOData a) => IO Handle -> Producer m a
sourceIOHandle alloc = bracketP alloc hClose sourceHandle

sinkHandle :: (MonadIO m, IOData a) => Handle -> Consumer a m ()
sinkHandle = awaitForever . hPut

sinkIOHandle :: (MonadResource m, IOData a) => IO Handle -> Consumer a m ()
sinkIOHandle alloc = bracketP alloc hClose sinkHandle

sinkFile :: (MonadResource m, IOData a) => FilePath -> Consumer a m ()
sinkFile fp = sinkIOHandle (F.openFile fp SIO.WriteMode)

lazySource :: (Monad m, LazySequence lazy strict) => lazy -> Producer m strict
lazySource = mapM_ yield . toChunks

lazySink :: (Monad m, LazySequence lazy strict) => Consumer strict m lazy
lazySink = fmap fromChunks CL.consume

foldLines :: (Monad m, Element c ~ Char, IsSequence c)
          => (a -> ConduitM c o m a)
          -> a
          -> ConduitM c o m a
foldLines f =
    start
  where
    start a = CL.peek >>= maybe (return a) (const $ loop $ f a)

    loop consumer = do
        a <- takeWhileC (/= '\n') =$= do
            a <- filterC (/= '\r') =$= consumer
            CL.sinkNull
            return a
        _ <- headC
        start a
