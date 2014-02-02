{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Spec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Arbitrary)
import Data.MonoTraversable
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as VS
import Data.Sequences
import Prelude (Bool (..), ($), IO, min, abs, Eq (..), (&&), fromIntegral, Ord (..), String, mod, Int, show,
                return, asTypeOf, (.), Show, id)
import qualified Prelude
import Control.Monad.Trans.Writer
import qualified Data.NonNull as NN
import qualified Data.List.NonEmpty as NE
import qualified Data.Semigroup as SG

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
    describe "NonNull" $ do
        let test' forceTyp typ dummy = describe typ $ do
                prop "head" $ \x xs ->
                    let nn = forceTyp $ NN.ncons x (fromList xs `asTypeOf` dummy)
                     in NN.head nn `shouldBe` x
                prop "tail" $ \x xs ->
                    let nn = forceTyp $ NN.ncons x (fromList xs `asTypeOf` dummy)
                     in NN.tail nn `shouldBe` fromList xs
                prop "last" $ \x xs ->
                    let nn = reverse $ forceTyp $ NN.ncons x (fromList xs `asTypeOf` dummy)
                     in NN.last nn `shouldBe` x
                prop "init" $ \x xs ->
                    let nn = reverse $ forceTyp $ NN.ncons x (fromList xs `asTypeOf` dummy)
                     in NN.init nn `shouldBe` reverse (fromList xs)
                prop "maximum" $ \x xs ->
                    let nn = reverse $ forceTyp $ NN.ncons x (fromList xs `asTypeOf` dummy)
                     in NN.maximum nn `shouldBe` Prelude.maximum (x:xs)
                prop "maximumBy" $ \x xs ->
                    let nn = reverse $ forceTyp $ NN.ncons x (fromList xs `asTypeOf` dummy)
                     in NN.maximumBy compare nn `shouldBe` Prelude.maximum (x:xs)
                prop "minimum" $ \x xs ->
                    let nn = reverse $ forceTyp $ NN.ncons x (fromList xs `asTypeOf` dummy)
                     in NN.minimum nn `shouldBe` Prelude.minimum (x:xs)
                prop "minimumBy" $ \x xs ->
                    let nn = reverse $ forceTyp $ NN.ncons x (fromList xs `asTypeOf` dummy)
                     in NN.minimumBy compare nn `shouldBe` Prelude.minimum (x:xs)
                prop "ofoldMap1" $ \x xs ->
                    let nn = forceTyp $ NN.ncons x (fromList xs `asTypeOf` dummy)
                     in SG.getMax (NN.ofoldMap1 SG.Max nn) `shouldBe` Prelude.maximum (x:xs)
                prop "ofoldr1" $ \x xs ->
                    let nn = forceTyp $ NN.ncons x (fromList xs `asTypeOf` dummy)
                     in NN.ofoldr1 (Prelude.min) nn `shouldBe` Prelude.minimum (x:xs)
                prop "ofoldl1'" $ \x xs ->
                    let nn = forceTyp $ NN.ncons x (fromList xs `asTypeOf` dummy)
                     in NN.ofoldl1' (Prelude.min) nn `shouldBe` Prelude.minimum (x:xs)

            test :: (OrdSequence typ, Arbitrary (Element typ), Show (Element typ), Show typ, Eq typ, Eq (Element typ))
                 => String -> typ -> Spec
            test = test' NN.asNotEmpty
        test "strict ByteString" S.empty
        test "lazy ByteString" L.empty
        test "strict Text" T.empty
        test "lazy Text" TL.empty
        test "Vector" (V.empty :: V.Vector Int)
        test "unboxed Vector" (U.empty :: U.Vector Int)
        test "storable Vector" (VS.empty :: VS.Vector Int)
        test "list" ([5 :: Int])
        test' (id :: NE.NonEmpty Int -> NE.NonEmpty Int) "NonEmpty" ([] :: [Int])
