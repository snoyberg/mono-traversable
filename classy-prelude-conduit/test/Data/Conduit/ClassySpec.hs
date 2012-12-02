module Data.Conduit.ClassySpec where

import Test.Hspec
import Data.Conduit.Classy
import qualified Data.Conduit.List as CL

spec :: Spec
spec = do
    describe "connecting" $ do
        it "works" $ do
            let sink :: Int -> Sink Char IO Int
                sink i = await >>= maybe (return i) (const $ sink $ i + 1)
            let str = "hello world"
            x <- mapM_ yield str $$ sink 0
            x `shouldBe` length str
    describe "connect-and-resume" $ do
        it "works" $ do
            let src :: Source IO Int
                src = mapM_ yield [1..30]
                take' = Sink . CL.take
            (r1, x) <- src $$+ take' 10
            (r2, y) <- r1 $$++ take' 10
            z <- r2 $$+- Sink CL.consume
            [x, y, z] `shouldBe` [[1..10], [11..20], [21..30]]
