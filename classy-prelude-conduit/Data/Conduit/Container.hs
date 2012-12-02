{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
-- | Note: This module is experimental, and might be modified at any time.
-- Caveat emptor!
module Data.Conduit.Container where

import Prelude ((.), Maybe (..), Monad (..), fmap, maybe, seq, Either (..), const, either, (-), ($), Int, compare, Ordering (..), id)
import qualified Prelude
import Data.Conduit.Classy
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Word (Word8)
import Control.Monad (liftM)

class Container c where
    type Single c
    type Multi c

    toSource :: (IsPipe m, PipeOutput m ~ c) => Multi c -> m ()

    headE :: (IsPipe m, PipeInput m ~ c) => m (Either (PipeTerm m) (Single c))
    head :: (IsPipe m, PipeInput m ~ c) => m (Maybe (Single c))
    head = liftM (either (const Nothing) Just) headE

    fold :: (IsPipe m, PipeInput m ~ c) => (accum -> Single c -> accum) -> accum -> m accum
    fold f =
        loop
      where
        loop accum =
            head >>= maybe (return accum) go
          where
            go a =
                let accum' = f accum a
                 in accum' `seq` loop accum'

    foldM :: (IsPipe m, PipeInput m ~ c) => (accum -> Single c -> m accum) -> accum -> m accum
    foldM f =
        loop
      where
        loop accum =
            head >>= maybe (return accum) go
          where
            go a = do
                accum' <- f accum a
                accum' `seq` loop accum'

    mapM_ :: (IsPipe m, PipeInput m ~ c) => (Single c -> m ()) -> m (PipeTerm m)
    mapM_ f =
        loop
      where
        loop = headE >>= either return (\s -> f s >> loop)

    drop :: (IsPipe m, PipeInput m ~ c) => Int -> m ()
    drop 0 = return ()
    drop i = head >>= maybe (return ()) (const $ drop (i - 1))

    singleton :: Single c -> c
    isolate :: (IsPipe m, PipeInput m ~ c, PipeOutput m ~ c) => Int -> m ()
    isolate 0 = return ()
    isolate i = head >>= maybe (return ()) (\x -> yield (singleton x) >> isolate (i - 1))
    consume :: (IsPipe m, PipeInput m ~ c) => m (Multi c)
    take :: (IsPipe m, PipeInput m ~ c) => Int -> m (Multi c)

instance Container S.ByteString where
    type Single S.ByteString = Word8
    type Multi S.ByteString = L.ByteString

    toSource = Prelude.mapM_ yield . L.toChunks

    headE = do
        ebs <- awaitE
        case ebs of
            Left t -> return (Left t)
            Right bs ->
                case S.uncons bs of
                    Nothing -> headE
                    Just (w, bs') -> leftover bs' >> return (Right w)

    fold f =
        loop
      where
        loop accum =
            await >>= maybe (return accum) go
          where
            go bs =
                let accum' = S.foldl' f accum bs
                 in accum' `seq` loop accum'

    mapM_ f =
        loop
      where
        loop = awaitE >>= either return (\bs -> Prelude.mapM_ f (S.unpack bs) >> loop)

    drop 0 = return ()
    drop i = await >>= maybe (return ()) (\bs ->
        case i `compare` S.length bs of
            LT -> leftover $ S.drop i bs
            EQ -> return ()
            GT -> drop (i - S.length bs))

    singleton = S.singleton
    consume =
        loop id
      where
        loop front = await >>= maybe (return $ L.fromChunks $ front []) (\bs -> loop $ front . (bs:))

    take =
        loop id
      where
        loop front 0 = return $ L.fromChunks $ front []
        loop front i = await >>= maybe (return $ L.fromChunks $ front []) (\bs ->
            case i `compare` S.length bs of
                LT -> do
                    let (x, y) = S.splitAt i bs
                    leftover y
                    return $ L.fromChunks $ front [x]
                EQ -> return $ L.fromChunks $ front [bs]
                GT -> loop (front . (bs:)) (i - S.length bs))

newtype Singleton a = Singleton { unSingleton :: a }

instance Container (Singleton a) where
    type Single (Singleton a) = a
    type Multi (Singleton a) = [a]

    toSource = Prelude.mapM_ (yield . Singleton)

    headE = liftM (fmap unSingleton) awaitE

    singleton = Singleton
    consume =
        loop id
      where
        loop front = head >>= maybe (return (front [])) (\x -> loop (front . (x:)))
    take =
        loop id
      where
        loop front 0 = return (front [])
        loop front i = head >>= maybe (return (front [])) (\x -> loop (front . (x:)) (i - 1))
