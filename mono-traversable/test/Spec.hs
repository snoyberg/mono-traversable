{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Spec where

import Data.MonoTraversable
import Data.Containers
import Data.Sequences
import qualified Data.Sequence as Seq
import qualified Data.NonNull as NN
import Data.Monoid (mempty, mconcat)
import Data.Maybe (fromMaybe)

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.HUnit ((@?=))
import Test.QuickCheck hiding (NonEmptyList(..))
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Modifiers as QCM

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as VS
import qualified Data.List.NonEmpty as NE
import qualified Data.Semigroup as SG
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import qualified Control.Foldl as Foldl

import Control.Arrow (first, second)
import Control.Applicative
import Control.Monad.Trans.Writer

import Prelude (Bool (..), ($), IO, min, abs, Eq (..), (&&), fromIntegral, Ord (..), String, mod, Int, Integer, show,
                return, asTypeOf, (.), Show, id, (+), succ, Maybe (..), (*), mod, map, flip, otherwise, (-), div, seq, maybe)
import qualified Prelude

instance Arbitrary a => Arbitrary (NE.NonEmpty a) where
    arbitrary = (NE.:|) <$> arbitrary <*> arbitrary

-- | Arbitrary newtype for key-value pairs without any duplicate keys
-- and is not empty
newtype DuplPairs k v = DuplPairs { unDupl :: [(k,v)] }
    deriving (Eq, Show)

removeDuplicateKeys :: Ord k => [(k,v)] -> [(k,v)]
removeDuplicateKeys m  = go Set.empty m
    where go _ [] = []
          go used ((k,v):xs)
            | k `member` used = go used xs
            | otherwise       = (k,v) : go (insertSet k used) xs

instance (Arbitrary k, Arbitrary v, Ord k, Eq v) => Arbitrary (DuplPairs k v) where
    arbitrary = DuplPairs . removeDuplicateKeys <$> arbitrary `suchThat` (/= [])
    shrink (DuplPairs xs) =
        map (DuplPairs . removeDuplicateKeys) $ filter (/= []) $ shrink xs

-- | Arbitrary newtype for small lists whose length is <= 10
--
-- Used for testing 'unionsWith'
newtype SmallList a = SmallList { getSmallList :: [a] }
    deriving (Eq, Show, Ord)

instance (Arbitrary a) => Arbitrary (SmallList a) where
    arbitrary = SmallList <$> arbitrary `suchThat` ((<= 10) . olength)
    shrink (SmallList xs) =
        map SmallList $ filter ((<= 10) . olength) $ shrink xs

-- | Choose a random key from a key-value pair list
indexIn :: (Show k, Testable prop) => [(k,v)] -> (k -> prop) -> Property
indexIn = forAll . elements . map Prelude.fst

-- | Type restricted 'fromList'
fromListAs :: IsSequence a => [Element a] -> a -> a
fromListAs xs _ = fromList xs

-- | Type restricted 'mapFromListAs'
mapFromListAs :: IsMap a => [(ContainerKey a, MapValue a)] -> a -> a
mapFromListAs xs _ = mapFromList xs

main :: IO ()
main = hspec $ do
    describe "onull" $ do
        it "works on empty lists"     $ onull []              @?= True
        it "works on non-empty lists" $ onull [()]            @?= False
        it "works on empty texts"     $ onull ("" :: Text)    @?= True
        it "works on non-empty texts" $ onull ("foo" :: Text) @?= False

    describe "osum" $ do
        prop "works on lists" $ \(Small x) (Small y) ->
            y >= x ==> osum [x..y] @?= ((x + y) * (y - x + 1) `div` 2)

    describe "oproduct" $ do
        prop "works on lists" $ \(Positive x) (Positive y) ->
            let fact n = oproduct [1..n]
             in (y :: Integer) > (x :: Integer) ==>
                    oproduct [x..y] @?= fact y `div` fact (x - 1)

    describe "olength" $ do
        prop "works on lists" $ \(NonNegative i) ->
            olength (replicate i () :: [()]) @?= i
        prop "works on texts" $ \(NonNegative i) ->
            olength (replicate i 'a' :: Text) @?= i
        prop "works on lazy bytestrings" $ \(NonNegative (Small i)) ->
            olength64 (replicate i 6 :: L.ByteString) @?= i

    describe "omap" $ do
        prop "works on lists" $ \xs ->
            omap (+1) xs @?= map (+1) (xs :: [Int])
        prop "works on lazy bytestrings" $ \xs ->
            omap (+1) (fromList xs :: L.ByteString) @?= fromList (map (+1) xs)
        prop "works on texts" $ \xs ->
            omap succ (fromList xs :: Text) @?= fromList (map succ xs)

    describe "oconcatMap" $ do
        prop "works on lists" $ \xs ->
            oconcatMap (: []) xs @?= (xs :: [Int])

    describe "ocompareLength" $ do
        prop "works on lists" $ \(Positive i) j ->
            ocompareLength (replicate i () :: [()]) j @?= compare i j

    describe "groupAll" $ do
        it "works on lists" $ groupAll ("abcabcabc" :: String) @?= ["aaa", "bbb", "ccc"]
        it "works on texts" $ groupAll ("abcabcabc" :: Text)   @?= ["aaa", "bbb", "ccc"]

    describe "unsnoc" $ do
        let test name dummy = prop name $ \(QCM.NonEmpty xs) ->
                let seq' = fromListAs xs dummy
                 in case unsnoc seq' of
                        Just (y, z) -> do
                            y SG.<> singleton z @?= seq'
                            snoc y z            @?= seq'
                            otoList (snoc y z)  @?= xs
                        Nothing -> expectationFailure "unsnoc returned Nothing"
        test "works on lists" ([] :: [Int])
        test "works on texts" ("" :: Text)
        test "works on lazy bytestrings" L.empty

    describe "index" $ do
        let test name dummy = prop name $
              \(NonNegative i') (QCM.NonEmpty xs) ->
                let seq' = fromListAs xs dummy
                    mx   = index xs (fromIntegral i)
                    i    = fromIntegral (i' :: Int)
                 in do
                    mx @?= index seq' i
                    case mx of
                        Nothing -> return ()
                        Just x  -> indexEx seq' i @?= x
        test "works on lists" ([] :: [Int])
        test "works on strict texts" ("" :: Text)
        test "works on lazy texts" ("" :: TL.Text)
        test "works on strict bytestrings" S.empty
        test "works on lazy bytestrings" L.empty
        test "works on Vector" (V.singleton (1 :: Int))
        test "works on SVector" (VS.singleton (1 :: Int))
        test "works on UVector" (U.singleton (1 :: Int))
        test "works on Seq" (Seq.fromList [1 :: Int])

    describe "groupAllOn" $ do
        it "works on lists" $
            groupAllOn (`mod` 3) ([1..9] :: [Int]) @?= [[1, 4, 7], [2, 5, 8], [3, 6, 9]]

    describe "breakWord" $ do
        let test x y z = it (show (x, y, z)) $ breakWord (x :: Text) @?= (y, z)
        test "hello world" "hello" "world"
        test "hello     world" "hello" "world"
        test "hello\r\nworld" "hello" "world"
        test "hello there  world" "hello" "there  world"
        test "" "" ""
        test "hello    \n\r\t" "hello" ""

    describe "breakLine" $ do
        let test x y z = it (show (x, y, z)) $ breakLine (x :: Text) @?= (y, z)
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
                input @?= execWriter (omapM_ (tell . return) (fromListAs input dummy))
        test "works on strict bytestrings" S.empty
        test "works on lazy bytestrings" L.empty
        test "works on strict texts" T.empty
        test "works on lazy texts" TL.empty

    describe "NonNull" $ do
        describe "fromNonEmpty" $ do
            prop "toMinList" $ \ne ->
                (NE.toList ne :: [Int]) @?= NN.toNullable (NN.toMinList ne)

        let -- | Type restricted 'NN.ncons'
            nconsAs :: IsSequence seq => Element seq -> [Element seq] -> seq -> NN.NonNull seq
            nconsAs x xs _ = NN.ncons x (fromList xs)

            test :: (OrdSequence typ, Arbitrary (Element typ), Show (Element typ), Show typ, Eq typ, Eq (Element typ))
                 => String -> typ -> Spec
            test typ du = describe typ $ do
                prop "head" $ \x xs ->
                    NN.head (nconsAs x xs du) @?= x
                prop "tail" $ \x xs ->
                    NN.tail (nconsAs x xs du) @?= fromList xs
                prop "last" $ \x xs ->
                    NN.last (reverse $ nconsAs x xs du) @?= x
                prop "init" $ \x xs ->
                    NN.init (reverse $ nconsAs x xs du) @?= reverse (fromList xs)
                prop "maximum" $ \x xs ->
                    NN.maximum (nconsAs x xs du) @?= Prelude.maximum (x:xs)
                prop "maximumBy" $ \x xs ->
                    NN.maximumBy compare (nconsAs x xs du) @?= Prelude.maximum (x:xs)
                prop "minimum" $ \x xs ->
                    NN.minimum (nconsAs x xs du) @?= Prelude.minimum (x:xs)
                prop "minimumBy" $ \x xs ->
                    NN.minimumBy compare (nconsAs x xs du) @?= Prelude.minimum (x:xs)
                prop "ofoldMap1" $ \x xs ->
                    SG.getMax (NN.ofoldMap1 SG.Max $ nconsAs x xs du) @?= Prelude.maximum (x:xs)
                prop "ofoldr1" $ \x xs ->
                    NN.ofoldr1 Prelude.min (nconsAs x xs du) @?= Prelude.minimum (x:xs)
                prop "ofoldl1'" $ \x xs ->
                    NN.ofoldl1' Prelude.min (nconsAs x xs du) @?= Prelude.minimum (x:xs)

        test "Strict ByteString" S.empty
        test "Lazy ByteString" L.empty
        test "Strict Text" T.empty
        test "Lazy Text" TL.empty
        test "Vector" (V.empty :: V.Vector Int)
        test "Unboxed Vector" (U.empty :: U.Vector Int)
        test "Storable Vector" (VS.empty :: VS.Vector Int)
        test "List" ([5 :: Int])

    describe "Containers" $ do
        let test typ dummy xlookup xinsert xdelete = describe typ $ do
                prop "difference" $ \(DuplPairs xs) (DuplPairs ys) ->
                    let m1 = mapFromList xs `difference` mapFromList ys
                        m2 = mapFromListAs (xs `difference` ys) dummy
                     in m1 @?= m2

                prop "lookup" $ \(DuplPairs xs) -> indexIn xs $ \k ->
                    let m = mapFromListAs xs dummy
                        v1 = lookup k m
                    in do
                        v1 @?= lookup k xs
                        v1 @?= xlookup k m

                prop "insert" $ \(DuplPairs xs) v -> indexIn xs $ \k ->
                    let m = mapFromListAs xs dummy
                        m1 = insertMap k v m
                     in do
                        m1 @?= mapFromList (insertMap k v xs)
                        m1 @?= xinsert k v m

                prop "delete" $ \(DuplPairs xs) -> indexIn xs $ \k ->
                    let m = mapFromListAs xs dummy
                        m1 = deleteMap k m
                     in do
                        m1 @?= mapFromList (deleteMap k xs)
                        m1 @?= xdelete k m

                prop "singletonMap" $ \k v ->
                    singletonMap k v @?= (mapFromListAs [(k, v)] dummy)

                prop "findWithDefault" $ \(DuplPairs xs) k v ->
                    findWithDefault v k (mapFromListAs xs dummy)
                        @?= findWithDefault v k xs

                prop "insertWith" $ \(DuplPairs xs) k v ->
                    insertWith (+) k v (mapFromListAs xs dummy)
                        @?= mapFromList (insertWith (+) k v xs)

                prop "insertWithKey" $ \(DuplPairs xs) k v ->
                    let m = mapFromListAs xs dummy
                        f x y z = x + y + z
                     in insertWithKey f k v m
                            @?= mapFromList (insertWithKey f k v xs)

                prop "insertLookupWithKey" $ \(DuplPairs xs) k v ->
                    let m = mapFromListAs xs dummy
                        f x y z = x + y + z
                     in insertLookupWithKey f k v m @?=
                            second mapFromList (insertLookupWithKey f k v xs)

                prop "adjustMap" $ \(DuplPairs xs) k ->
                    adjustMap succ k (mapFromListAs xs dummy)
                        @?= mapFromList (adjustMap succ k xs)

                prop "adjustWithKey" $ \(DuplPairs xs) k ->
                    adjustWithKey (+) k (mapFromListAs xs dummy)
                        @?= mapFromList (adjustWithKey (+) k xs)

                prop "updateMap" $ \(DuplPairs xs) k ->
                    let f i = if i < 0 then Nothing else Just $ i * 2
                     in updateMap f k (mapFromListAs xs dummy)
                            @?= mapFromList (updateMap f k xs)

                prop "updateWithKey" $ \(DuplPairs xs) k ->
                    let f k i = if i < 0 then Nothing else Just $ i * k
                     in updateWithKey f k (mapFromListAs xs dummy)
                            @?= mapFromList (updateWithKey f k xs)

                prop "updateLookupWithKey" $ \(DuplPairs xs) k ->
                    let f k i = if i < 0 then Nothing else Just $ i * k
                     in updateLookupWithKey f k (mapFromListAs xs dummy)
                            @?= second mapFromList (updateLookupWithKey f k xs)

                prop "alter" $ \(DuplPairs xs) k ->
                    let m = mapFromListAs xs dummy
                        f Nothing = Just (-1)
                        f (Just i) = if i < 0 then Nothing else Just (i * 2)
                     in lookup k (alterMap f k m) @?= f (lookup k m)

                prop "unionWith" $ \(DuplPairs xs) (DuplPairs ys) ->
                    let m1 = unionWith (+)
                                (mapFromListAs xs dummy)
                                (mapFromListAs ys dummy)
                        m2 = mapFromList (unionWith (+) xs ys)
                     in m1 @?= m2

                prop "unionWithKey" $ \(DuplPairs xs) (DuplPairs ys) ->
                    let f k x y = k + x + y
                        m1 = unionWithKey f
                                (mapFromListAs xs dummy)
                                (mapFromListAs ys dummy)
                        m2 = mapFromList (unionWithKey f xs ys)
                     in m1 @?= m2

                prop "unionsWith" $ \(SmallList xss) ->
                    let duplXss = map unDupl xss
                        ms = map mapFromList duplXss `asTypeOf` [dummy]
                     in unionsWith (+) ms
                            @?= mapFromList (unionsWith (+) duplXss)

                prop "mapWithKey" $ \(DuplPairs xs) ->
                    let m1 = mapWithKey (+) (mapFromList xs) `asTypeOf` dummy
                        m2 = mapFromList $ mapWithKey (+) xs
                     in m1 @?= m2

                prop "omapKeysWith" $ \(DuplPairs xs) ->
                    let f = flip mod 5
                        m1 = omapKeysWith (+) f (mapFromList xs) `asTypeOf` dummy
                        m2 = mapFromList $ omapKeysWith (+) f xs
                     in m1 @?= m2

        test "Data.Map" (Map.empty :: Map.Map Int Int)
            Map.lookup Map.insert Map.delete
        test "Data.IntMap" (IntMap.empty :: IntMap.IntMap Int)
            IntMap.lookup IntMap.insert IntMap.delete
        test "Data.HashMap" (HashMap.empty :: HashMap.HashMap Int Int)
            HashMap.lookup HashMap.insert HashMap.delete

    describe "Foldl Integration" $ do
        prop "vector" $ \xs -> do
            x1 <- Foldl.foldM Foldl.vector (xs :: [Int])
            x2 <- Foldl.impurely ofoldMUnwrap Foldl.vector xs
            x2 @?= (x1 :: V.Vector Int)
        prop "length" $ \xs -> do
            let x1 = Foldl.fold Foldl.length (xs :: [Int])
                x2 = Foldl.purely ofoldlUnwrap Foldl.length xs
            x2 @?= x1

    describe "Sorting" $ do
        let test typ dummy = describe typ $ do
                prop "sortBy" $ \input -> do
                    let f x y = compare y x
                    fromList (sortBy f input) @?= sortBy f (fromListAs input dummy)
                prop "sort" $ \input ->
                    fromList (sort input) @?= sort (fromListAs input dummy)
        test "List" ([] :: [Int])
        test "Vector" (V.empty :: V.Vector Int)
        test "Storable Vector" (VS.empty :: VS.Vector Int)
        test "Unboxed Vector" (U.empty :: U.Vector Int)
        test "Strict ByteString" S.empty
        test "Lazy ByteString" L.empty
        test "Strict Text" T.empty
        test "Lazy Text" TL.empty

    describe "Intercalate" $ do
        let test typ dummy = describe typ $ do
                prop "intercalate === defaultIntercalate" $ \list lists ->
                    let seq = fromListAs list dummy
                        seqs = map (`fromListAs` dummy) lists
                    in intercalate seq seqs @?= defaultIntercalate seq seqs
        test "List" ([] :: [Int])
        test "Vector" (V.empty :: V.Vector Int)
        test "Storable Vector" (VS.empty :: VS.Vector Int)
        test "Unboxed Vector" (U.empty :: U.Vector Int)
        test "Strict ByteString" S.empty
        test "Lazy ByteString" L.empty
        test "Strict Text" T.empty
        test "Lazy Text" TL.empty

    describe "Splitting" $ do
        let test typ dummy = describe typ $ do
                let fromList' = (`fromListAs` dummy)
                let fromSepList sep = fromList' . map (fromMaybe sep)
                prop "intercalate sep . splitSeq sep === id" $
                    \(fromList' -> sep) ->
                    \(mconcat . map (maybe sep fromList') -> xs) ->
                    intercalate sep (splitSeq sep xs) @?= xs
                prop "splitSeq mempty xs === mempty : map singleton (otoList xs)" $
                    \input ->
                    splitSeq mempty (fromList' input) @?= mempty : map singleton input
                prop "splitSeq _ mempty == [mempty]" $
                    \(fromList' -> sep) ->
                    splitSeq sep mempty @?= [mempty]
                prop "intercalate (singleton sep) . splitElem sep === id" $
                    \sep -> \(fromSepList sep -> xs) ->
                    intercalate (singleton sep) (splitElem sep xs) @?= xs
                prop "length . splitElem sep === succ . length . filter (== sep)" $
                    \sep -> \(fromSepList sep -> xs) ->
                    olength (splitElem sep xs) @?= olength (filter (== sep) xs) + 1
                prop "splitElem sep (replicate n sep) == replicate (n+1) mempty" $
                    \(NonNegative n) sep ->
                    splitElem sep (fromList' (replicate n sep)) @?= replicate (n + 1) mempty
                prop "splitElem sep === splitWhen (== sep)" $
                    \sep -> \(fromSepList sep -> xs) ->
                    splitElem sep xs @?= splitWhen (== sep) xs
                prop "splitElem sep === splitSeq (singleton sep)" $
                    \sep -> \(fromSepList sep -> xs) ->
                    splitElem sep xs @?= splitSeq (singleton sep) xs
        test "List" ([] :: [Int])
        test "Vector" (V.empty :: V.Vector Int)
        test "Storable Vector" (VS.empty :: VS.Vector Int)
        test "Unboxed Vector" (U.empty :: U.Vector Int)
        test "Strict ByteString" S.empty
        test "Lazy ByteString" L.empty
        test "Strict Text" T.empty
        test "Lazy Text" TL.empty

    describe "Other Issues" $ do
        it "#26 headEx on a list works" $
            headEx (1 : filter Prelude.odd [2,4..]) @?= (1 :: Int)

        it "#31 find doesn't infinitely loop on NonEmpty" $
            find (== "a") ("a" NE.:| ["d","fgf"]) @?= Just "a"

        it "#83 head on Seq works correctly" $ do
            headEx (Seq.fromList [1 :: Int,2,3]) @?= (1 :: Int)
            headMay (Seq.fromList [] :: Seq.Seq Int) @?= Nothing
