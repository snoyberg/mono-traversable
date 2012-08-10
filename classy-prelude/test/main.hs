{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Test.Hspec
import Test.Hspec.QuickCheck
import ClassyPrelude
import ClassyPrelude.Classes
import Test.QuickCheck.Arbitrary
import Prelude (asTypeOf, undefined)

dictionaryProps
    :: ( CanInsertVal a Int Char
       , CanDelete a Int
       , Show a
       , Eq a
       , Arbitrary a
       , CanEmpty a
       , CanLookup a Int Char
       )
    => a -> Spec
dictionaryProps dummy = do
    prop "insert x y (insert x z c) == insert x y c" $ \x y z c ->
        insert x y (insert x z (c `asTypeOf` dummy)) == insert x y c
    prop "insert x y (delete x c) == insert x y c" $ \x y c ->
        insert x y (delete x (c `asTypeOf` dummy)) == insert x y c
    prop "delete x (insert x y c) == delete x c" $ \x y c ->
        delete x (insert x y (c `asTypeOf` dummy)) == delete x c
    prop "lookup k (insert k v empty) == Just v" $ \k v ->
        lookup k (insert k v empty `asTypeOf` dummy) == Just v
    prop "lookup k (delete k c) == Nothing" $ \k c ->
        lookup k (delete k c`asTypeOf` dummy) == Nothing

main :: IO ()
main = hspec $ do
    describe "dictionary" $ do
        describe "Data.Map" $ dictionaryProps (undefined :: Map Int Char)
        describe "Data.HashMap" $ dictionaryProps (undefined :: HashMap Int Char)
        describe "assoc list" $ dictionaryProps (undefined :: [(Int, Char)])

instance Arbitrary (Map Int Char) where
    arbitrary = fromList <$> arbitrary
instance Arbitrary (HashMap Int Char) where
    arbitrary = fromList <$> arbitrary
