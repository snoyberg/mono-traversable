module Data.Conduit.ContainerSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary
import Data.Conduit.Classy
import qualified Data.Conduit.Container as C
import Data.Functor.Identity (Identity, runIdentity)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

spec :: Spec
spec = do
    describe "Singleton" $ do
        prop "consumes" $ \x ->
            runIdentity ((C.toSource x :: Source Identity (C.Singleton Int)) $$ C.consume) == x
        prop "takes" $ \str i' ->
            let x = (C.toSource str :: Source Identity (C.Singleton Char)) $$ C.take i
                i = abs i'
             in runIdentity x == take i str
    describe "ByteString" $ do
        prop "consumes" $ \(ArbLByteString x) ->
            runIdentity ((C.toSource x :: Source Identity S.ByteString) $$ C.consume) == x
        prop "takes" $ \(ArbLByteString str) i' ->
            let x = (C.toSource str :: Source Identity S.ByteString) $$ C.take i
                i = abs i'
             in runIdentity x == L.take (fromIntegral i) str

newtype ArbByteString = ArbByteString { unArbByteString :: S.ByteString }
    deriving Show

instance Arbitrary ArbByteString where
    arbitrary = fmap (ArbByteString . S.pack) arbitrary

newtype ArbLByteString = ArbLByteString L.ByteString
    deriving Show

instance Arbitrary ArbLByteString where
    arbitrary = fmap (ArbLByteString . L.fromChunks . map unArbByteString) arbitrary
