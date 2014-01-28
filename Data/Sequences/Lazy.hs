{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Data.Sequences.Lazy where

import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import           Data.Sequences
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL

-- | Lazy sequences containing strict chunks of data.
class (IsSequence lazy, IsSequence strict) => LazySequence lazy strict | lazy -> strict, strict -> lazy where
    toChunks :: lazy -> [strict]
    fromChunks :: [strict] -> lazy
    toStrict :: lazy -> strict
    fromStrict :: strict -> lazy

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
