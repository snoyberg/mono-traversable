module Data.Conduit.ClassySpec where

import Test.Hspec
import Data.Conduit.Classy

spec :: Spec
spec = do
    describe "connecting" $ do
        it "works" $ do
            let sink :: Int -> Sink Char IO Int
                sink i = await >>= maybe (return i) (const $ sink $ i + 1)
            let str = "hello world"
            x <- mapM_ yield str $$ sink 0
            x `shouldBe` length str
