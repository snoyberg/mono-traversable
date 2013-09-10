{-# LANGUAGE OverloadedStrings #-}
module Spec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Data.MonoTraversable
import Data.Text (Text)
import qualified Data.ByteString.Lazy as L

main :: IO ()
main = hspec $ do
    describe "cnull" $ do
        it "empty list" $ cnull [] `shouldBe` True
        it "non-empty list" $ cnull [()] `shouldBe` False
        it "empty text" $ cnull ("" :: Text) `shouldBe` True
        it "non-empty text" $ cnull ("foo" :: Text) `shouldBe` False
    describe "clength" $ do
        prop "list" $ \i' ->
            let x = creplicate i () :: [()]
                i = min 500 $ abs i'
             in clength x == i &&
                clength64 x == fromIntegral i
        prop "text" $ \i' ->
            let x = creplicate i 'a' :: Text
                i = min 500 $ abs i'
             in clength x == i &&
                clength64 x == fromIntegral i
        prop "lazy bytestring" $ \i' ->
            let x = creplicate i 6 :: L.ByteString
                i = min 500 $ abs i'
             in clength x == i &&
                clength64 x == fromIntegral i
