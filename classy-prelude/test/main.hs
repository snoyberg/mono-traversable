{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Test.Hspec
import Test.Hspec.QuickCheck
import ClassyPrelude
import ClassyPrelude.Classes
import Test.QuickCheck.Arbitrary
import Prelude (asTypeOf, undefined)
import qualified Prelude

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

mapProps :: ( CanPack a i
            , CanPack b j
            , Eq a
            , Show a
            , Arbitrary a
            , Eq b
            , Show b
            , Arbitrary b
            , CanMapFunc a b i j
            )
         => a
         -> (i -> j)
         -> Spec
mapProps dummy f = do
    prop "map f c == pack (map f (unpack c))" $ \c ->
        map f (c `asTypeOf` dummy) == pack (map f (unpack c))

concatMapProps :: ( CanPack a i
            , CanPack b j
            , CanPack js j
            , Eq a
            , Show a
            , Arbitrary a
            , Eq b
            , Show b
            , Arbitrary b
            , CanMapFunc a b i j
            , CanConcatMapFunc a b i js
            )
         => a
         -> (i -> js)
         -> Spec
concatMapProps dummy f = do
    prop "concatMap f c == pack (concatMap (unpack . f) (unpack c))" $ \c ->
        concatMap f (c `asTypeOf` dummy) == pack (concatMap (unpack . f) (unpack c))

filterProps :: ( CanPack a i
               , Show a
               , Arbitrary a
               , Eq a
               , CanFilterFunc a a i
               )
            => a
            -> (i -> Bool)
            -> Spec
filterProps dummy f = do
    prop "filter f c == pack (filter f (unpack c))" $ \c ->
        filter f (c `asTypeOf` dummy) == pack (filter f (unpack c))

main :: IO ()
main = hspec $ do
    describe "dictionary" $ do
        describe "Data.Map" $ dictionaryProps (undefined :: Map Int Char)
        describe "Data.HashMap" $ dictionaryProps (undefined :: HashMap Int Char)
        describe "assoc list" $ dictionaryProps (undefined :: [(Int, Char)])
    describe "map" $ do
        describe "list" $ mapProps (undefined :: [Int]) (+ 1)
        describe "Data.Vector" $ mapProps (undefined :: Vector Int) (+ 1)
        describe "Data.Set" $ mapProps (undefined :: Set Int) (+ 1)
        describe "Data.HashSet" $ mapProps (undefined :: HashSet Int) (+ 1)
        describe "Data.ByteString" $ mapProps (undefined :: ByteString) (+ 1)
        describe "Data.ByteString.Lazy" $ mapProps (undefined :: LByteString) (+ 1)
        describe "Data.Text" $ mapProps (undefined :: Text) succ
        describe "Data.Text.Lazy" $ mapProps (undefined :: LText) succ
    describe "concatMap" $ do
        describe "list" $ concatMapProps (undefined :: [Int]) (\i -> [i + 1, i + 2])
        describe "Data.Vector" $ concatMapProps (undefined :: Vector Int) (\i -> fromList [i + 1, i + 2])
        describe "Data.ByteString" $ concatMapProps (undefined :: ByteString) (\i -> fromList [i + 1, i + 2])
        describe "Data.ByteString.Lazy" $ concatMapProps (undefined :: LByteString) (\i -> fromList [i + 1, i + 2])
        describe "Data.Text" $ concatMapProps (undefined :: Text) (\c -> pack [succ c, succ $ succ c])
        describe "Data.Text.Lazy" $ concatMapProps (undefined :: LText) (\c -> pack [succ c, succ $ succ c])
    describe "filter" $ do
        describe "list" $ filterProps (undefined :: [Int]) (< 20)
        describe "Data.Vector" $ filterProps (undefined :: Vector Int) (< 20)
        describe "Data.ByteString" $ filterProps (undefined :: ByteString) (< 20)
        describe "Data.ByteString.Lazy" $ filterProps (undefined :: LByteString) (< 20)
        describe "Data.Text" $ filterProps (undefined :: Text) (< 'A')
        describe "Data.Text.Lazy" $ filterProps (undefined :: LText) (< 'A')
        describe "Data.Map" $ filterProps (undefined :: Map Int Char) (\(i, _) -> i < 20)
        describe "Data.HashMap" $ filterProps (undefined :: HashMap Int Char) (\(i, _) -> i < 20)

instance Arbitrary (Map Int Char) where
    arbitrary = fromList <$> arbitrary
instance Arbitrary (HashMap Int Char) where
    arbitrary = fromList <$> arbitrary
instance Arbitrary (Vector Int) where
    arbitrary = fromList <$> arbitrary
instance Arbitrary (Set Int) where
    arbitrary = fromList <$> arbitrary
instance Arbitrary (HashSet Int) where
    arbitrary = fromList <$> arbitrary
instance Arbitrary ByteString where
    arbitrary = fromList <$> arbitrary
instance Arbitrary LByteString where
    arbitrary = fromList <$> arbitrary
instance Arbitrary Text where
    arbitrary = fromList <$> arbitrary
instance Arbitrary LText where
    arbitrary = fromList <$> arbitrary
