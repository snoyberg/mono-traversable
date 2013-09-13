{-# LANGUAGE OverloadedStrings #-}
module Spec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Data.MonoTraversable
import Data.Text (Text)
import qualified Data.ByteString.Lazy as L
import Data.Sequences
import Prelude (Bool (..), ($), IO, min, abs, Eq (..), (&&), fromIntegral, Ord (..), String)

main :: IO ()
main = hspec $ do
    describe "cnull" $ do
        it "empty list" $ onull [] `shouldBe` True
        it "non-empty list" $ onull [()] `shouldBe` False
        it "empty text" $ onull ("" :: Text) `shouldBe` True
        it "non-empty text" $ onull ("foo" :: Text) `shouldBe` False
    describe "clength" $ do
        prop "list" $ \i' ->
            let x = replicate i () :: [()]
                i = min 500 $ abs i'
             in olength x == i
        prop "text" $ \i' ->
            let x = replicate i 'a' :: Text
                i = min 500 $ abs i'
             in olength x == i
        prop "lazy bytestring" $ \i' ->
            let x = replicate i 6 :: L.ByteString
                i = min 500 $ abs i'
             in olength64 x == i
    describe "ccompareLength" $ do
        prop "list" $ \i' j ->
            let i = min 500 $ abs i'
                x = replicate i () :: [()]
             in ocompareLength x j == compare i j
    describe "group'" $ do
        it "list" $ group' ("abcabcabc" :: String) == ["aaa", "bbb", "ccc"]
        it "Text" $ group' ("abcabcabc" :: Text) == ["aaa", "bbb", "ccc"]
