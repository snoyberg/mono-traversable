{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
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
                return, asTypeOf, (.), Show, id, (+), succ, Maybe (..), (*), mod, map, flip, otherwise, (-), div, seq)
import qualified Prelude
import Control.Monad.Trans.Writer
import qualified Data.NonNull as NN
import qualified Data.List.NonEmpty as NE
import qualified Data.Semigroup as SG
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.HashMap.Strict as HashMap
import Data.Containers
import qualified Data.IntSet as IntSet
import Control.Arrow (first, second)
import qualified Control.Foldl as Foldl
import Data.Int (Int64)

main :: IO ()
main = hspec $ do
    describe "cnull" $ do
        it "empty list" $ onull [] `shouldBe` True
        it "non-empty list" $ onull [()] `shouldBe` False
        it "empty text" $ onull ("" :: Text) `shouldBe` True
        it "non-empty text" $ onull ("foo" :: Text) `shouldBe` False
    describe "osum" $ do
        it "list" $ do
            let x = 1
                -- explicitly using Int64 to avoid overflow issues, see:
                -- https://github.com/snoyberg/mono-traversable/issues/29
                y = 10000000 :: Int64
                list = [x..y]
            osum list `shouldBe` ((x + y) * (y - x + 1) `div` 2)
    describe "oproduct" $ do
        it "list" $ do
            let x = 1
                y = 10000000 :: Int64
                list = [x..y]
                fact n =
                    go 1 1
                  where
                    go i j
                        | i `seq` j `seq` j >= n = i
                        | otherwise = go (i * j) (j + 1)
            oproduct list `shouldBe` fact y `div` (fact (x - 1))
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
    describe "unsnoc" $ do
        let test name dummy = prop name $ \xs ->
                let seq' = fromList xs `asTypeOf` dummy
                 in case (unsnoc seq', onull seq', onull xs) of
                        (Nothing, True, True) -> return ()
                        (Just (y, z), False, False) -> do
                            (y SG.<> singleton z) `shouldBe` seq'
                            snoc y z `shouldBe` seq'
                            otoList (snoc y z) `shouldBe` xs
                        x -> Prelude.error $ show x
        test "list" ([] :: [Int])
        test "Text" ("" :: Text)
        test "lazy ByteString" L.empty
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
            test = test' id
        test "strict ByteString" S.empty
        test "lazy ByteString" L.empty
        test "strict Text" T.empty
        test "lazy Text" TL.empty
        test "Vector" (V.empty :: V.Vector Int)
        test "unboxed Vector" (U.empty :: U.Vector Int)
        test "storable Vector" (VS.empty :: VS.Vector Int)
        test "list" ([5 :: Int])
        -- test' (id :: NE.NonEmpty Int -> NE.NonEmpty Int) "NonEmpty" ([] :: [Int])

    describe "Containers" $ do
        let test typ dummy xlookup xinsert xdelete = describe typ $ do
                prop "difference" $ \(filterDups -> xs) (filterDups -> ys) -> do
                    let m1 = mapFromList xs `difference` mapFromList ys
                        m2 = mapFromList (xs `difference` ys) `asTypeOf` dummy
                    m1 `shouldBe` m2
                prop "lookup" $ \(fixK -> k) (filterDups -> xs) -> do
                    let m = mapFromList xs
                        v1 = lookup k m
                        v2 = lookup k (xs :: [(Int, Int)])
                        v3 = xlookup k m
                    v1 `shouldBe` v2
                    v1 `shouldBe` v3
                prop "insert" $ \(fixK -> k) v (filterDups -> xs) -> do
                    let m = mapFromList (xs :: [(Int, Int)])
                        m1 = insertMap k v m
                        m2 = mapFromList (insertMap k v xs)
                        m3 = xinsert k v m
                    m1 `shouldBe` m2
                    m1 `shouldBe` m3
                prop "delete" $ \(fixK -> k) (filterDups -> xs) -> do
                    let m = mapFromList (xs :: [(Int, Int)]) `asTypeOf` dummy
                        m1 = deleteMap k m
                        m2 = mapFromList (deleteMap k xs)
                        m3 = xdelete k m
                    m1 `shouldBe` m2
                    m1 `shouldBe` m3
                prop "singletonMap" $ \(fixK -> k) v -> do
                    singletonMap k v `shouldBe` (mapFromList [(k, v)] `asTypeOf` dummy)
                prop "findWithDefault" $ \(fixK -> k) v (filterDups -> xs) -> do
                    let m = mapFromList xs `asTypeOf` dummy
                    findWithDefault v k m `shouldBe` findWithDefault v k xs
                prop "insertWith" $ \(fixK -> k) v (filterDups -> xs) -> do
                    let m = mapFromList xs `asTypeOf` dummy
                        f = (+)
                    insertWith f k v m `shouldBe` mapFromList (insertWith f k v xs)
                prop "insertWithKey" $ \(fixK -> k) v (filterDups -> xs) -> do
                    let m = mapFromList xs `asTypeOf` dummy
                        f x y z = x + y + z
                    insertWithKey f k v m `shouldBe` mapFromList (insertWithKey f k v xs)
                prop "insertLookupWithKey" $ \(fixK -> k) v (filterDups -> xs) -> do
                    let m = mapFromList xs `asTypeOf` dummy
                        f x y z = x + y + z
                    insertLookupWithKey f k v m `shouldBe`
                        second mapFromList (insertLookupWithKey f k v xs)
                prop "adjustMap" $ \(fixK -> k) (filterDups -> xs) -> do
                    let m = mapFromList xs `asTypeOf` dummy
                    adjustMap succ k m `shouldBe` mapFromList (adjustMap succ k xs)
                prop "adjustWithKey" $ \(fixK -> k) (filterDups -> xs) -> do
                    let m = mapFromList xs `asTypeOf` dummy
                    adjustWithKey (+) k m `shouldBe` mapFromList (adjustWithKey (+) k xs)
                prop "updateMap" $ \(fixK -> k) (filterDups -> xs) -> do
                    let m = mapFromList xs `asTypeOf` dummy
                        f i = if i < 0 then Nothing else Just $ i * 2
                    updateMap f k m `shouldBe` mapFromList (updateMap f k xs)
                prop "updateWithKey" $ \(fixK -> k) (filterDups -> xs) -> do
                    let m = mapFromList xs `asTypeOf` dummy
                        f k i = if i < 0 then Nothing else Just $ i * k
                    updateWithKey f k m `shouldBe` mapFromList (updateWithKey f k xs)
                prop "updateLookupWithKey" $ \(fixK -> k) (filterDups -> xs) -> do
                    let m = mapFromList xs `asTypeOf` dummy
                        f k i = if i < 0 then Nothing else Just $ i * k
                    updateLookupWithKey f k m `shouldBe` second mapFromList (updateLookupWithKey f k xs)
                prop "alter" $ \(fixK -> k) (filterDups -> xs) -> do
                    let m = mapFromList xs `asTypeOf` dummy
                        f Nothing = Just (-1)
                        f (Just i) = if i < 0 then Nothing else Just (i * 2)
                    lookup k (alterMap f k m) `shouldBe` f (lookup k m)
                prop "unionWith" $ \(filterDups -> xs) (filterDups -> ys) -> do
                    let m1 = unionWith (+)
                                (mapFromList xs `asTypeOf` dummy)
                                (mapFromList ys `asTypeOf` dummy)
                        m2 = mapFromList (unionWith (+) xs ys)
                    m1 `shouldBe` m2
                prop "unionWithKey" $ \(filterDups -> xs) (filterDups -> ys) -> do
                    let f k x y = k + x + y
                        m1 = unionWithKey f
                                (mapFromList xs `asTypeOf` dummy)
                                (mapFromList ys `asTypeOf` dummy)
                        m2 = mapFromList (unionWithKey f xs ys)
                    m1 `shouldBe` m2
                prop "unionsWith" $ \(map filterDups -> xss) -> do
                    let ms = map mapFromList xss `asTypeOf` [dummy]
                    unionsWith (+) ms `shouldBe` mapFromList (unionsWith (+) xss)
                prop "mapWithKey" $ \(filterDups -> xs) -> do
                    let m1 = mapWithKey (+) (mapFromList xs) `asTypeOf` dummy
                        m2 = mapFromList $ mapWithKey (+) xs
                    m1 `shouldBe` m2
                prop "mapKeysWith" $ \(filterDups -> xs) -> do
                    let m1 = mapKeysWith (+) f (mapFromList xs) `asTypeOf` dummy
                        m2 = mapFromList $ mapKeysWith (+) f xs
                        f = flip mod 5
                    m1 `shouldBe` m2
            filterDups :: [(Int, v)] -> [(Int, v)]
            filterDups =
                loop IntSet.empty . map (first (`mod` 20))
              where
                loop _ [] = []
                loop used ((k, v):rest)
                    | k `member` used = loop used rest
                    | Prelude.otherwise = (k, v) : loop (insertSet k used) rest

            fixK :: Int -> Int
            fixK = flip mod 20

        test "Data.Map" Map.empty Map.lookup Map.insert Map.delete
        test "Data.IntMap" IntMap.empty IntMap.lookup IntMap.insert IntMap.delete
        test "Data.HashMap" HashMap.empty HashMap.lookup HashMap.insert HashMap.delete

    describe "foldl integration" $ do
        prop "vector" $ \xs -> do
            x1 <- Foldl.foldM Foldl.vector (xs :: [Int])
            x2 <- Foldl.impurely ofoldMUnwrap Foldl.vector xs
            x2 `shouldBe` (x1 :: V.Vector Int)
        prop "length" $ \xs -> do
            let x1 = Foldl.fold Foldl.length (xs :: [Int])
                x2 = Foldl.purely ofoldlUnwrap Foldl.length xs
            x2 `shouldBe` x1

    describe "sorting" $ do
        let test typ dummy = describe typ $ do
                prop "sortBy" $ \input -> do
                    let orig = fromList input `asTypeOf` dummy
                        f x y = compare y x
                    fromList (sortBy f input) `shouldBe` sortBy f orig
                    fromList input `shouldBe` orig
                prop "sort" $ \input -> do
                    let orig = fromList input `asTypeOf` dummy
                    fromList (sort input) `shouldBe` sort orig
                    fromList input `shouldBe` orig
        test "list" ([] :: [Int])
        test "vector" (V.empty :: V.Vector Int)
        test "storable vector" (VS.empty :: VS.Vector Int)
        test "unboxed vector" (U.empty :: U.Vector Int)
        test "strict ByteString" S.empty
        test "lazy ByteString" L.empty
        test "strict Text" T.empty
        test "lazy Text" TL.empty
    it "headEx on a list works #26" $
        headEx (1 : filter Prelude.odd [2,4..]) `shouldBe` (1 :: Int)
