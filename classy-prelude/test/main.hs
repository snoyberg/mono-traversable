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
import Prelude (asTypeOf, undefined, fromIntegral)
import qualified Prelude
import Control.Monad.Trans.Writer (tell, Writer, runWriter)
import Data.Maybe (isJust)
import Data.Functor.Identity (runIdentity)

dictionaryProps
    :: ( CanInsertVal a Int Char
       , CanDeleteVal a Int
       , Show a
       , Eq a
       , Arbitrary a
       , Monoid a
       , CanLookup a Int Char
       , CanPack a (Int, Char)
       )
    => a -> Spec
dictionaryProps dummy = do
    prop "insert x y (insert x z c) == insert x y c" $ \x y z c ->
        insert x y (insert x z (c `asTypeOf` dummy)) == insert x y c
    prop "insert x y (delete x c) == insert x y c" $ \x y c ->
        insert x y (delete x (c `asTypeOf` dummy)) == insert x y c
    prop "delete x (insert x y c) == delete x c" $ \x y c ->
        pack (unpack $ delete x (insert x y (c `asTypeOf` dummy))) == (pack (unpack ((delete x c) `asTypeOf` dummy) :: [(Int, Char)]) `asTypeOf` dummy)
    prop "lookup k (insert k v empty) == Just v" $ \k v ->
        lookup k (insert k v empty `asTypeOf` dummy) == Just v
    prop "lookup k (delete k c) == Nothing" $ \k c ->
        lookup k (delete k c`asTypeOf` dummy) == Nothing

mapProps :: ( CanPack a i
            , CanPack b j
            , Eq a
            , Eq c
            , Show a
            , Arbitrary a
            , Eq b
            , Show b
            , Arbitrary b
            , CanMapFunc a b i j
            , CanMapFunc a c i k
            , CanMapFunc b c j k
            )
         => a
         -> (i -> j)
         -> (j -> k)
         -> Spec
mapProps dummy f g = do
    prop "map f c == pack (map f (unpack c))" $ \c ->
        map f (c `asTypeOf` dummy) == pack (map f (unpack c))
    prop "map (f . g) c == map f (map g c)" $ \c ->
        map (g . f) (c `asTypeOf` dummy) == map g (map f c)

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
        (repack (filter f (c `asTypeOf` dummy)) `asTypeOf` dummy) == pack (filter f (unpack c))

filterMProps :: ( CanPack a i
                , Show a
                , Arbitrary a
                , Eq a
                , CanFilterMFunc a i
                )
             => a
             -> (i -> Bool)
             -> Spec
filterMProps dummy f' = do
    prop "filterM f c == fmap pack (filterM f (unpack c))" $ \c ->
        runIdentity (fmap repack (filterM f (c `asTypeOf` dummy))) `asTypeOf` dummy == runIdentity (fmap pack (filterM f (unpack c)))
  where
    f = return . f'

lengthProps :: ( Show a
               , Eq a
               , Arbitrary a
               , CanPack a i
               , CanLength a len
               , Prelude.Num len
               , Eq len
               , CanNull a
               , Ord len
               , Monoid a
               )
            => a -> Spec
lengthProps dummy = do
    prop "length c == fromIntegral (length (unpack c))" $ \c ->
        length (c `asTypeOf` dummy) == fromIntegral (length (unpack c))
    prop "null c == (length c == 0)" $ \c ->
        null (c `asTypeOf` dummy) == (length c == 0)
    prop "length (x ++ y) <= length x + length y" $ \x y ->
        length (x ++ y `asTypeOf` dummy) <= length x + length y
    prop "length (x ++ y) >= max (length x) (length y)" $ \x y ->
        length (x ++ y `asTypeOf` dummy) >= max (length x) (length y)
    prop "length (x ++ empty) == length x" $ \x ->
        length (x ++ empty `asTypeOf` dummy) == length x
    prop "null empty" $ null (empty `asTypeOf` dummy)

{-
mapMProps :: ( Show a
             , Arbitrary a
             , CanPack a i
             , Eq i
             , CanMapMFunc a co i i
             , CanPack co i
             , Eq co
             )
           => a -> Spec
-}
mapMProps dummy = do
    let f :: Int -> Writer [Int] Int
        f x = tell [x] >> return x
    prop "mapM f c == mapM f (toList c)" $ \c ->
        runWriter (mapM f (c `asTypeOf` dummy)) ==
            let (x, y) = runWriter (mapM f (toList c))
             in (pack x, y)

mapM_Props :: ( Show a
              , Arbitrary a
              , CanPack a i
              , Eq i
              , CanMapM_Func a i
              )
           => a -> Spec
mapM_Props dummy = do
    let f x = tell [x]
    prop "mapM_ f c == mapM_ f (toList c)" $ \c ->
        runWriter (mapM_ f (c `asTypeOf` dummy)) == runWriter (mapM_ f (toList c))

foldProps dummy f accum =
    prop "fold f accum c == fold f accum (toList c)" $ \c ->
        fold f accum (c `asTypeOf` dummy) == fold f accum (toList c)

replicateProps :: ( Show a
                  , Eq a
                  , CanReplicate a i len
                  , Integral len
                  , Show len
                  , Arbitrary len
                  , Show i
                  , Arbitrary i
                  )
               => a -> ([i] -> a) -> Spec
replicateProps dummy pack' =
    prop "replicate i a == pack (replicate i a)" $ \{- takes too long i-} a ->
        (replicate i a `asTypeOf` dummy) == pack' (replicate (fromIntegral i) a)
  where
    i = 3

chunkProps :: ( Eq a
              , Show a
              , Arbitrary a
              , CanToChunks a i
              , Monoid i
              ) => a -> Spec
chunkProps dummy = do
    prop "fromChunks . toChunks == id" $ \a ->
        fromChunks (toChunks (a `asTypeOf` dummy)) == a
    prop "fromChunks . return . concat . toChunks == id" $ \a ->
        fromChunks [concat $ toChunks (a `asTypeOf` dummy)] == a

stripSuffixProps :: ( Eq a
                    , Monoid a
                    , CanStripSuffix a
                    , Show a
                    , Arbitrary a
                    ) => a -> Spec
stripSuffixProps dummy = do
    prop "stripSuffix y (x ++ y) == Just x" $ \x y ->
        stripSuffix y (x ++ y) == Just (x `asTypeOf` dummy)
    prop "isJust (stripSuffix x y) == isSuffixOf x y" $ \x y ->
        isJust (stripSuffix x y) == isSuffixOf x (y `asTypeOf` dummy)

replicateMProps :: ( Eq c
                   , Show len
                   , Arbitrary len
                   , CanReplicateM c i len
                   , CanReplicate c i len
                   , Show i
                   , Arbitrary i
                   , Integral len
                   ) => c -> Spec
replicateMProps dummy = do
    prop "runIdentity (replicateM i (return x)) == replicate i x" $ \i' x ->
        let i = i' `mod` 20
         in runIdentity (replicateM i (return x)) == (replicate i x `asTypeOf` dummy)

utf8Props :: ( Eq t
             , Show t
             , Arbitrary t
             , CanEncodeUtf8Func t b
             , CanDecodeUtf8Func b t
             ) => t -> Spec
utf8Props dummy = do
    prop "decodeUtf8 . encodeUtf8 == id" $ \t ->
        decodeUtf8 (encodeUtf8 t) == (t `asTypeOf` dummy)

main :: IO ()
main = hspec $ do
    describe "dictionary" $ do
        describe "Data.Map" $ dictionaryProps (undefined :: Map Int Char)
        describe "Data.HashMap" $ dictionaryProps (undefined :: HashMap Int Char)
        describe "assoc list" $ dictionaryProps (undefined :: [(Int, Char)])
    describe "map" $ do
        describe "list" $ mapProps (undefined :: [Int]) (+ 1) (+ 2)
        describe "Data.Vector" $ mapProps (undefined :: Vector Int) (+ 1) (+ 2)
        describe "Data.Set" $ mapProps (undefined :: Set Int) (+ 1) (+ 2)
        describe "Data.HashSet" $ mapProps (undefined :: HashSet Int) (+ 1) (+ 2)
        describe "Data.ByteString" $ mapProps (undefined :: ByteString) (+ 1) (+ 2)
        describe "Data.ByteString.Lazy" $ mapProps (undefined :: LByteString) (+ 1) (+ 2)
        describe "Data.Text" $ mapProps (undefined :: Text) succ succ
        describe "Data.Text.Lazy" $ mapProps (undefined :: LText) succ succ
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
    describe "filterM" $ do
        describe "list" $ filterMProps (undefined :: [Int]) (< 20)
        describe "Data.Vector" $ filterMProps (undefined :: Vector Int) (< 20)
    describe "length" $ do
        describe "list" $ lengthProps (undefined :: [Int])
        describe "Data.Vector" $ lengthProps (undefined :: Vector Int)
        describe "Data.ByteString" $ lengthProps (undefined :: ByteString)
        describe "Data.ByteString.Lazy" $ lengthProps (undefined :: LByteString)
        describe "Data.Text" $ lengthProps (undefined :: Text)
        describe "Data.Text.Lazy" $ lengthProps (undefined :: LText)
        describe "Data.Map" $ lengthProps (undefined :: Map Int Char)
        describe "Data.HashMap" $ lengthProps (undefined :: HashMap Int Char)
        describe "Data.Set" $ lengthProps (undefined :: Set Int)
        describe "Data.HashSet" $ lengthProps (undefined :: HashSet Int)
    describe "mapM" $ do
        describe "list" $ mapMProps (undefined :: [Int])
        describe "Data.Vector" $ mapMProps (undefined :: Vector Int)
    describe "mapM_" $ do
        describe "list" $ mapM_Props (undefined :: [Int])
        describe "Data.Vector" $ mapM_Props (undefined :: Vector Int)
        describe "Set" $ mapM_Props (undefined :: Set Int)
        describe "HashSet" $ mapM_Props (undefined :: HashSet Int)
    describe "fold" $ do
        let f = flip (:)
        describe "list" $ foldProps (undefined :: [Int]) f []
        describe "Data.Vector" $ foldProps (undefined :: Vector Int) f []
        describe "Data.ByteString" $ foldProps (undefined :: ByteString) f []
        describe "Data.ByteString.Lazy" $ foldProps (undefined :: LByteString) f []
        describe "Data.Text" $ foldProps (undefined :: Text) f []
        describe "Data.Text.Lazy" $ foldProps (undefined :: LText) f []
        describe "Data.Set" $ foldProps (undefined :: Set Int) f []
        describe "Data.HashSet" $ foldProps (undefined :: HashSet Int) f []
    describe "replicate" $ do
        describe "list" $ replicateProps (undefined :: [Int]) pack
        describe "Data.Vector" $ replicateProps (undefined :: Vector Int) pack
        describe "Data.ByteString" $ replicateProps (undefined :: ByteString) pack
        describe "Data.ByteString.Lazy" $ replicateProps (undefined :: LByteString) pack
        describe "Data.Text" $ replicateProps (undefined :: Text) concat
        describe "Data.Text.Lazy" $ replicateProps (undefined :: LText) concat
    describe "chunks" $ do
        describe "ByteString" $ chunkProps (asLByteString undefined)
        describe "Text" $ chunkProps (asLText undefined)
    describe "stripSuffix" $ do
        describe "Text" $ stripSuffixProps (undefined :: Text)
        describe "LText" $ stripSuffixProps (undefined :: LText)
        describe "ByteString" $ stripSuffixProps (undefined :: ByteString)
        describe "LByteString" $ stripSuffixProps (undefined :: LByteString)
    describe "replicateM" $ do
        describe "list" $ replicateMProps (undefined :: [Int])
        describe "Vector" $ replicateMProps (undefined :: Vector Int)
    describe "encode/decode UTF8" $ do
        describe "Text" $ utf8Props (undefined :: Text)
        describe "LText" $ utf8Props (undefined :: LText)

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
