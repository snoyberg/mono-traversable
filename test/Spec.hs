{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Conduit
import Test.Hspec
import Test.Hspec.QuickCheck
import BasicPrelude
import qualified Data.Text.Lazy as TL
import Data.IORef
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

main :: IO ()
main = hspec $ do
    describe "yieldMany" $ do
        it "list" $
            runIdentity (yieldMany [1..10] $$ sinkList)
            `shouldBe` [1..10]
        it "Text" $
            runIdentity (yieldMany ("Hello World" :: Text) $$ sinkList)
            `shouldBe` "Hello World"
    it "unfold" $
        let f 11 = Nothing
            f i = Just (show i, i + 1)
         in runIdentity (unfoldC f 1 $$ sinkList)
            `shouldBe` map show [1..10]
    it "enumFromTo" $
        runIdentity (enumFromToC 1 10 $$ sinkList) `shouldBe` [1..10]
    it "iterate" $
        let f i = i + 1
            src = iterateC f seed
            seed = 1
            count = 10
            res = runIdentity $ src $$ takeC count =$ sinkList
         in res `shouldBe` take count (iterate f seed)
    it "repeat" $
        let src = repeatC seed
            seed = 1
            count = 10
            res = runIdentity $ src $$ takeC count =$ sinkList
         in res `shouldBe` take count (repeat seed)
    it "replicate" $
        let src = replicateC count seed
            seed = 1
            count = 10
            res = runIdentity $ src $$ sinkList
         in res `shouldBe` replicate count seed
    it "sourceLazy" $
        let tss = ["foo", "bar", "baz"]
            tl = TL.fromChunks tss
            res = runIdentity $ sourceLazy tl $$ sinkList
         in res `shouldBe` tss
    it "repeatM" $
        let src = repeatMC (return seed)
            seed = 1
            count = 10
            res = runIdentity $ src $$ takeC count =$ sinkList
         in res `shouldBe` take count (repeat seed)
    it "repeatWhileM" $ do
        ref <- newIORef 0
        let f = atomicModifyIORef ref $ \i -> (succ i, succ i)
            src = repeatWhileMC f (< 11)
        res <- src $$ sinkList
        res `shouldBe` [1..10]
    it "replicateM" $ do
        ref <- newIORef 0
        let f = atomicModifyIORef ref $ \i -> (succ i, succ i)
            src = replicateMC 10 f
        res <- src $$ sinkList
        res `shouldBe` [1..10]
    it "sourceFile" $ do
        let contents = "this is some content"
            fp = "tmp"
        writeFile fp contents
        res <- runResourceT $ sourceFile fp $$ sinkLazy
        res `shouldBe` TL.fromStrict contents
    it "drop" $
        runIdentity (yieldMany [1..10] $$ (dropC 5 >> sinkList))
        `shouldBe` [6..10]
    it "dropE" $
        runIdentity (yield ("Hello World" :: Text) $$ (dropCE 5 >> sinkLazy))
        `shouldBe` " World"
    it "dropWhile" $
        runIdentity (yieldMany [1..10] $$ (dropWhileC (<= 5) >> sinkList))
        `shouldBe` [6..10]
    it "dropWhileE" $
        runIdentity (yield ("Hello World" :: Text) $$ (dropWhileCE (/= 'W') >> sinkLazy))
        `shouldBe` "World"
    it "fold" $
        let list = [[1..10], [11..20]]
            src = yieldMany list
            res = runIdentity $ src $$ foldC
         in res `shouldBe` concat list
    it "foldE" $
        let list = [[1..10], [11..20]]
            src = yieldMany $ Identity list
            res = runIdentity $ src $$ foldCE
         in res `shouldBe` concat list
    it "foldl" $
        let res = runIdentity $ yieldMany [1..10] $$ foldlC (+) 0
         in res `shouldBe` sum [1..10]
    it "foldlE" $
        let res = runIdentity $ yield [1..10] $$ foldlCE (+) 0
         in res `shouldBe` sum [1..10]
    it "foldMap" $
        let src = yieldMany [1..10]
            res = runIdentity $ src $$ foldMapC return
         in res `shouldBe` [1..10]
    it "foldMapE" $
        let src = yield [1..10]
            res = runIdentity $ src $$ foldMapCE return
         in res `shouldBe` [1..10]
    prop "all" $ \input -> runIdentity (yieldMany input $$ allC even) `shouldBe` all evenInt input
    prop "allE" $ \input -> runIdentity (yield input $$ allCE even) `shouldBe` all evenInt input
    prop "any" $ \input -> runIdentity (yieldMany input $$ anyC even) `shouldBe` any evenInt input
    prop "anyE" $ \input -> runIdentity (yield input $$ anyCE even) `shouldBe` any evenInt input
    prop "and" $ \input -> runIdentity (yieldMany input $$ andC) `shouldBe` and input
    prop "andE" $ \input -> runIdentity (yield input $$ andCE) `shouldBe` and input
    prop "or" $ \input -> runIdentity (yieldMany input $$ orC) `shouldBe` or input
    prop "orE" $ \input -> runIdentity (yield input $$ orCE) `shouldBe` or input
    prop "elem" $ \x xs -> runIdentity (yieldMany xs $$ elemC x) `shouldBe` elemInt x xs
    prop "elemE" $ \x xs -> runIdentity (yield xs $$ elemCE x) `shouldBe` elemInt x xs
    prop "notElem" $ \x xs -> runIdentity (yieldMany xs $$ notElemC x) `shouldBe` notElemInt x xs
    prop "notElemE" $ \x xs -> runIdentity (yield xs $$ notElemCE x) `shouldBe` notElemInt x xs
    prop "sinkVector regular" $ \xs' -> do
        let maxSize = 20
            xs = take maxSize xs'
        res <- yieldMany xs' $$ sinkVector maxSize
        res `shouldBe` V.fromList (xs :: [Int])
    prop "sinkVector unboxed" $ \xs' -> do
        let maxSize = 20
            xs = take maxSize xs'
        res <- yieldMany xs' $$ sinkVector maxSize
        res `shouldBe` VU.fromList (xs :: [Int])
    prop "sinkVector storable" $ \xs' -> do
        let maxSize = 20
            xs = take maxSize xs'
        res <- yieldMany xs' $$ sinkVector maxSize
        res `shouldBe` VS.fromList (xs :: [Int])

evenInt :: Int -> Bool
evenInt = even

elemInt :: Int -> [Int] -> Bool
elemInt = elem

notElemInt :: Int -> [Int] -> Bool
notElemInt = notElem
