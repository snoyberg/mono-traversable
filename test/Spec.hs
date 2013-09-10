{-# LANGUAGE OverloadedStrings #-}
module Spec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Data.MonoTraversable
import Data.Text (Text)
import qualified Data.ByteString.Lazy as L
import Data.Sequences
import Prelude (Bool (..), ($), IO, min, abs, Eq (..), (&&), fromIntegral, Ord (..))

main :: IO ()
main = hspec $ do
    describe "cnull" $ do
        it "empty list" $ cnull [] `shouldBe` True
        it "non-empty list" $ cnull [()] `shouldBe` False
        it "empty text" $ cnull ("" :: Text) `shouldBe` True
        it "non-empty text" $ cnull ("foo" :: Text) `shouldBe` False
    describe "clength" $ do
        prop "list" $ \i' ->
            let x = replicate i () :: [()]
                i = min 500 $ abs i'
             in clength x == i &&
                clength64 x == fromIntegral i
        prop "text" $ \i' ->
            let x = replicate i 'a' :: Text
                i = min 500 $ abs i'
             in clength x == i &&
                clength64 x == fromIntegral i
        prop "lazy bytestring" $ \i' ->
            let x = replicate i 6 :: L.ByteString
                i = min 500 $ abs i'
             in clength x == i &&
                clength64 x == fromIntegral i
    describe "ccompareLength" $ do
        prop "list" $ \i' j ->
            let i = min 500 $ abs i'
                x = replicate i () :: [()]
             in ccompareLength x j == compare i j