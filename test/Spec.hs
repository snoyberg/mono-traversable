{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Conduit
import Test.Hspec
import Test.Hspec.QuickCheck
import BasicPrelude hiding (encodeUtf8)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.IORef
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import Control.Monad.Trans.Writer
import qualified Prelude
import qualified System.IO as IO
import Data.Builder
import Data.Sequences.Lazy
import Data.Textual.Encoding

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
        let contents = concat $ replicate 10000 $ "this is some content\n"
            fp = "tmp"
        writeFile fp contents
        res <- runResourceT $ sourceFile fp $$ sinkLazy
        res `shouldBe` TL.fromStrict contents
    it "sourceHandle" $ do
        let contents = concat $ replicate 10000 $ "this is some content\n"
            fp = "tmp"
        writeFile fp contents
        res <- IO.withBinaryFile "tmp" IO.ReadMode $ \h -> sourceHandle h $$ sinkLazy
        res `shouldBe` TL.fromStrict contents
    it "sourceIOHandle" $ do
        let contents = concat $ replicate 10000 $ "this is some content\n"
            fp = "tmp"
        writeFile fp contents
        let open = IO.openBinaryFile "tmp" IO.ReadMode
        res <- runResourceT $ sourceIOHandle open $$ sinkLazy
        res `shouldBe` TL.fromStrict contents
    prop "drop" $ \(T.pack -> input) count ->
        runIdentity (yieldMany input $$ (dropC count >>= \() -> sinkList))
        `shouldBe` T.unpack (T.drop count input)
    prop "dropE" $ \(T.pack -> input) ->
        runIdentity (yield input $$ (dropCE 5 >>= \() -> foldC))
        `shouldBe` T.drop 5 input
    prop "dropWhile" $ \(T.pack -> input) sep ->
        runIdentity (yieldMany input $$ (dropWhileC (<= sep) >>= \() -> sinkList))
        `shouldBe` T.unpack (T.dropWhile (<= sep) input)
    prop "dropWhileE" $ \(T.pack -> input) sep ->
        runIdentity (yield input $$ (dropWhileCE (<= sep) >>= \() -> foldC))
        `shouldBe` T.dropWhile (<= sep) input
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
    prop "sinkBuilder" $ \(map T.pack -> inputs) ->
        let builder = runIdentity (yieldMany inputs $$ sinkBuilder) :: TextBuilder
            ltext = builderToLazy builder
         in ltext `shouldBe` fromChunks inputs
    prop "sinkLazyBuilder" $ \(map T.pack -> inputs) ->
        let lbs = runIdentity (yieldMany inputs $$ sinkLazyBuilder)
         in lbs `shouldBe` encodeUtf8 (fromChunks inputs)
    prop "sinkNull" $ \xs toSkip -> do
        res <- yieldMany xs $$ do
            takeC toSkip =$ sinkNull
            sinkList
        res `shouldBe` drop toSkip (xs :: [Int])
    prop "mapM_" $ \xs ->
        let res = execWriter $ yieldMany xs $$ mapM_C (tell . return)
         in res `shouldBe` (xs :: [Int])
    prop "mapM_E" $ \xs ->
        let res = execWriter $ yield xs $$ mapM_CE (tell . return)
         in res `shouldBe` (xs :: [Int])
    prop "foldM" $ \xs -> do
        res <- yieldMany xs $$ foldMC addM 0
        res `shouldBe` sum xs
    prop "foldME" $ \xs -> do
        res <- yield xs $$ foldMCE addM 0
        res `shouldBe` sum xs
    it "foldMapM" $
        let src = yieldMany [1..10]
            res = runIdentity $ src $$ foldMapMC (return . return)
         in res `shouldBe` [1..10]
    it "foldMapME" $
        let src = yield [1..10]
            res = runIdentity $ src $$ foldMapMCE (return . return)
         in res `shouldBe` [1..10]
    it "sinkFile" $ do
        let contents = concat $ replicate 1000 $ "this is some content\n"
            fp = "tmp"
        runResourceT $ yield contents $$ sinkFile fp
        res <- readFile fp
        res `shouldBe` contents
    it "sinkHandle" $ do
        let contents = concat $ replicate 1000 $ "this is some content\n"
            fp = "tmp"
        IO.withBinaryFile "tmp" IO.WriteMode $ \h -> yield contents $$ sinkHandle h
        res <- readFile fp
        res `shouldBe` contents
    it "sinkIOHandle" $ do
        let contents = concat $ replicate 1000 $ "this is some content\n"
            fp = "tmp"
            open = IO.openBinaryFile "tmp" IO.WriteMode
        runResourceT $ yield contents $$ sinkIOHandle open
        res <- readFile fp
        res `shouldBe` contents
    prop "map" $ \input ->
        runIdentity (yieldMany input $$ mapC succChar =$ sinkList)
        `shouldBe` map succChar input
    prop "mapE" $ \(map V.fromList -> inputs) ->
        runIdentity (yieldMany inputs $$ mapCE succChar =$ foldC)
        `shouldBe` V.map succChar (V.concat inputs)
    prop "omapE" $ \(map T.pack -> inputs) ->
        runIdentity (yieldMany inputs $$ omapCE succChar =$ foldC)
        `shouldBe` T.map succChar (T.concat inputs)
    prop "concatMap" $ \input ->
        runIdentity (yieldMany input $$ concatMapC showInt =$ sinkList)
        `shouldBe` concatMap showInt input
    prop "concatMapE" $ \input ->
        runIdentity (yield input $$ concatMapCE showInt =$ foldC)
        `shouldBe` concatMap showInt input
    prop "take" $ \(T.pack -> input) count ->
        runIdentity (yieldMany input $$ (takeC count >>= \() -> mempty) =$ sinkList)
        `shouldBe` T.unpack (T.take count input)
    prop "takeE" $ \(T.pack -> input) count ->
        runIdentity (yield input $$ (takeCE count >>= \() -> mempty) =$ foldC)
        `shouldBe` T.take count input
    prop "takeWhile" $ \(T.pack -> input) sep ->
        runIdentity (yieldMany input $$ do
            x <- (takeWhileC (<= sep) >>= \() -> mempty) =$ sinkList
            y <- sinkList
            return (x, y))
        `shouldBe` span (<= sep) (T.unpack input)
    prop "takeWhileE" $ \(T.pack -> input) sep ->
        runIdentity (yield input $$ do
            x <- (takeWhileCE (<= sep) >>= \() -> mempty) =$ foldC
            y <- foldC
            return (x, y))
        `shouldBe` T.span (<= sep) input
    it "takeExactly" $
        let src = yieldMany [1..10]
            sink = do
                x <- takeExactlyC 5 $ return 1
                y <- sinkList
                return (x, y)
            res = runIdentity $ src $$ sink
         in res `shouldBe` (1, [6..10])
    it "takeExactlyE" $
        let src = yield ("Hello World" :: Text)
            sink = do
                takeExactlyCE 5 (mempty :: Sink Text Identity ())
                y <- sinkLazy
                return y
            res = runIdentity $ src $$ sink
         in res `shouldBe` " World"
    it "takeExactlyE Vector" $ do
        let src = yield (V.fromList $ T.unpack "Hello World")
            sink = do
                x <- takeExactlyCE 5 $ return 1
                y <- foldC
                return (x, y)
        res <- src $$ sink
        res `shouldBe` (1, V.fromList $ T.unpack " World")
    it "takeExactlyE 2" $
        let src = yield ("Hello World" :: Text)
            sink = do
                x <- takeExactlyCE 5 $ return 1
                y <- sinkLazy
                return (1, y)
            res = runIdentity $ src $$ sink
         in res `shouldBe` (1, " World")
    prop "concat" $ \input ->
        runIdentity (yield (T.pack input) $$ concatC =$ sinkList)
        `shouldBe` input
    prop "filter" $ \input ->
        runIdentity (yieldMany input $$ filterC evenInt =$ sinkList)
        `shouldBe` filter evenInt input
    prop "filterE" $ \input ->
        runIdentity (yield input $$ filterCE evenInt =$ foldC)
        `shouldBe` filter evenInt input
    prop "mapWhile" $ \input (min 20 -> highest) ->
        let f i =
                if i < highest
                    then Just (i + 2 :: Int)
                    else Nothing
            res = runIdentity $ yieldMany input $$ do
                x <- (mapWhileC f >>= \() -> mempty) =$ sinkList
                y <- sinkList
                return (x, y)
            expected = (map (+ 2) $ takeWhile (< highest) input, dropWhile (< highest) input)
         in res `shouldBe` expected
    prop "conduitVector" $ \(take 200 -> input) size' -> do
        let size = min 30 $ succ $ abs size'
        res <- yieldMany input $$ conduitVector size =$ sinkList
        res `shouldSatisfy` all (\v -> V.length v <= size)
        drop 1 (reverse res) `shouldSatisfy` all (\v -> V.length v == size)
        V.concat res `shouldBe` V.fromList (input :: [Int])
    prop "mapM" $ \input ->
        runIdentity (yieldMany input $$ mapMC (return . succChar) =$ sinkList)
        `shouldBe` map succChar input
    prop "mapME" $ \(map V.fromList -> inputs) ->
        runIdentity (yieldMany inputs $$ mapMCE (return . succChar) =$ foldC)
        `shouldBe` V.map succChar (V.concat inputs)
    prop "omapME" $ \(map T.pack -> inputs) ->
        runIdentity (yieldMany inputs $$ omapMCE (return . succChar) =$ foldC)
        `shouldBe` T.map succChar (T.concat inputs)
    prop "concatMapM" $ \input ->
        runIdentity (yieldMany input $$ concatMapMC (return . showInt) =$ sinkList)
        `shouldBe` concatMap showInt input
    prop "filterM" $ \input ->
        runIdentity (yieldMany input $$ filterMC (return . evenInt) =$ sinkList)
        `shouldBe` filter evenInt input
    prop "filterME" $ \input ->
        runIdentity (yield input $$ filterMCE (return . evenInt) =$ foldC)
        `shouldBe` filter evenInt input
    prop "iterM" $ \input -> do
        (x, y) <- runWriterT $ yieldMany input $$ iterMC (tell . return) =$ sinkList
        x `shouldBe` (input :: [Int])
        y `shouldBe` input

evenInt :: Int -> Bool
evenInt = even

elemInt :: Int -> [Int] -> Bool
elemInt = elem

notElemInt :: Int -> [Int] -> Bool
notElemInt = notElem

addM :: Monad m => Int -> Int -> m Int
addM x y = return (x + y)

succChar :: Char -> Char
succChar = succ

showInt :: Int -> String
showInt = Prelude.show
