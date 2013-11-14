{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Data.Sequences.Lazy where

import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import           Data.Sequences
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL

class (IsSequence l, IsSequence s) => LazySequence l s | l -> s, s -> l where
    toChunks :: l -> [s]
    fromChunks :: [s] -> l
    toStrict :: l -> s
    fromStrict :: s -> l

instance LazySequence L.ByteString S.ByteString where
    toChunks = L.toChunks
    fromChunks = L.fromChunks
    toStrict = S.concat . L.toChunks
    fromStrict = L.fromChunks . return

instance LazySequence TL.Text T.Text where
    toChunks = TL.toChunks
    fromChunks = TL.fromChunks
    toStrict = TL.toStrict
    fromStrict = TL.fromStrict
