{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
module Data.Textual.Encoding where

import qualified Data.ByteString          as S
import qualified Data.ByteString.Lazy     as L
import           Data.Sequences
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import           Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Encoding  as TL
import           Data.Word                (Word8)

class (Textual t, IsSequence b) => Utf8 t b | t -> b, b -> t where
    encodeUtf8 :: t -> b
    decodeUtf8 :: b -> t
instance (c ~ Char, w ~ Word8) => Utf8 [c] [w] where
    encodeUtf8 = L.unpack . TL.encodeUtf8 . TL.pack
    decodeUtf8 = TL.unpack . TL.decodeUtf8With lenientDecode . L.pack
instance Utf8 T.Text S.ByteString where
    encodeUtf8 = T.encodeUtf8
    decodeUtf8 = T.decodeUtf8With lenientDecode
instance Utf8 TL.Text L.ByteString where
    encodeUtf8 = TL.encodeUtf8
    decodeUtf8 = TL.decodeUtf8With lenientDecode
