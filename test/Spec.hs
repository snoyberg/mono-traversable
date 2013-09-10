{-# LANGUAGE OverloadedStrings #-}
module Spec where

import Test.Hspec
import Data.MonoTraversable
import Data.Text (Text)

main :: IO ()
main = hspec $ do
    describe "cnull" $ do
        it "empty list" $ cnull [] `shouldBe` True
        it "non-empty list" $ cnull [()] `shouldBe` False
        it "empty text" $ cnull ("" :: Text) `shouldBe` True
        it "non-empty text" $ cnull ("foo" :: Text) `shouldBe` False