{-# LANGUAGE OverloadedStrings #-}
module Spec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Data.MonoTraversable
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Sequences
import Prelude (Bool (..), ($), IO, min, abs, Eq (..), (&&), fromIntegral, Ord (..), String, mod, Int, show,
                return, asTypeOf, (.))
import Control.Monad.Trans.Writer
import qualified Data.NonNull as NN

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
    describe "groupAll" $ do
        it "list" $ groupAll ("abcabcabc" :: String) == ["aaa", "bbb", "ccc"]
        it "Text" $ groupAll ("abcabcabc" :: Text) == ["aaa", "bbb", "ccc"]
    describe "groupAllOn" $ do
        it "list" $ groupAllOn (`mod` 3) ([1..9] :: [Int]) == [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
    describe "breakWord" $ do
        let test x y z = it (show (x, y, z)) $ breakWord (x :: Text) `shouldBe` (y, z)
        test "hello world" "hello" "world"
        test "hello     world" "hello" "world"
        test "hello\r\nworld" "hello" "world"
        test "hello there  world" "hello" "there  world"
        test "" "" ""
        test "hello    \n\r\t" "hello" ""
    describe "breakLine" $ do
        let test x y z = it (show (x, y, z)) $ breakLine (x :: Text) `shouldBe` (y, z)
        test "hello world" "hello world" ""
        test "hello\r\n world" "hello" " world"
        test "hello\n world" "hello" " world"
        test "hello\r world" "hello\r world" ""
        test "hello\r\nworld" "hello" "world"
        test "hello\r\nthere\nworld" "hello" "there\nworld"
        test "hello\n\r\nworld" "hello" "\r\nworld"
        test "" "" ""
    describe "omapM_" $ do
        let test typ dummy = prop typ $ \input ->
                let res = execWriter $ omapM_ (tell . return) (fromList input `asTypeOf` dummy)
                 in res == input
        test "strict ByteString" S.empty
        test "lazy ByteString" L.empty
        test "strict Text" T.empty
        test "lazy Text" TL.empty
    describe "SafeSequence" $ do
        let test typ dummy = describe typ $ do
                prop "head" $ \x xs ->
                    let nn = NN.asNotEmpty $ NN.ncons x (fromList xs `asTypeOf` dummy)
                     in NN.head nn `shouldBe` x
                prop "tail" $ \x xs ->
                    let nn = NN.asNotEmpty $ NN.ncons x (fromList xs `asTypeOf` dummy)
                     in NN.tail nn `shouldBe` fromList xs
                prop "last" $ \x xs ->
                    let nn = reverse $ NN.asNotEmpty $ NN.ncons x (fromList xs `asTypeOf` dummy)
                     in NN.last nn `shouldBe` x
                prop "init" $ \x xs ->
                    let nn = reverse $ NN.asNotEmpty $ NN.ncons x (fromList xs `asTypeOf` dummy)
                     in NN.init nn `shouldBe` reverse (fromList xs)
        test "strict ByteString" S.empty
        test "lazy ByteString" L.empty
        test "strict Text" T.empty
        test "lazy Text" TL.empty
