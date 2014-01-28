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

-- | Textual data which can be encoded to and decoded from UTF8.
class (Textual textual, IsSequence binary) => Utf8 textual binary | textual -> binary, binary -> textual where
    encodeUtf8 :: textual -> binary
    -- | Note that this function is required to be pure. In the case of
    -- a decoding error, Unicode replacement characters must be used.
    decodeUtf8 :: binary -> textual
instance (c ~ Char, w ~ Word8) => Utf8 [c] [w] where
    encodeUtf8 = L.unpack . TL.encodeUtf8 . TL.pack
    decodeUtf8 = TL.unpack . TL.decodeUtf8With lenientDecode . L.pack
instance Utf8 T.Text S.ByteString where
    encodeUtf8 = T.encodeUtf8
    decodeUtf8 = T.decodeUtf8With lenientDecode
instance Utf8 TL.Text L.ByteString where
    encodeUtf8 = TL.encodeUtf8
    decodeUtf8 = TL.decodeUtf8With lenientDecode
