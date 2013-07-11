{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Test.Hspec
import Test.Hspec.QuickCheck
import ClassyPrelude
import ClassyPrelude.Classes
import Test.QuickCheck.Arbitrary
import Prelude (asTypeOf, fromIntegral)
import qualified Prelude
import Control.Monad.Trans.Writer (tell, Writer, runWriter)
import Data.Maybe (isJust)
import Data.Functor.Identity (runIdentity)
import Control.Concurrent (throwTo, threadDelay, forkIO)
import Control.Exception (throw)

dictionaryProps
    :: ( CanInsertVal a Int Char
       , CanDeleteVal a Int
       , CanEmpty a
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
            , CanMap a b i j
            , CanMap a c i k
            , CanMap b c j k
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
            , CanMap a b i j
            , CanConcatMap a b i js
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
               , CanFilter a i
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
                , CanFilterM a i
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
               , CanEmpty a
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
             , CanMapM a co i i
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
              , CanMapM_ a i
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
             , CanEncodeUtf8 t b
             , CanDecodeUtf8 b t
             ) => t -> Spec
utf8Props dummy = do
    prop "decodeUtf8 . encodeUtf8 == id" $ \t ->
        decodeUtf8 (encodeUtf8 t) == (t `asTypeOf` dummy)

compareLengthProps :: ( Show c
                      , Arbitrary c
                      , CanCompareLength c
                      , Show l
                      , Arbitrary l
                      , Integral l
                      , CanLength c l
                      ) => c -> Spec
compareLengthProps dummy = do
    prop "compare (length c) i == compareLength c i" $ \i c ->
        compare (length c) i == compareLength (c `asTypeOf` dummy) i

prefixProps :: ( Show c
               , Eq c
               , Monoid c
               , CanStripPrefix c
               , Arbitrary c
               )
            => c -> Spec
prefixProps dummy = do
    prop "x `isPrefixOf` (x ++ y)" $ \x y ->
        (x `asTypeOf` dummy) `isPrefixOf` (x ++ y)
    prop "stripPrefix x (x ++ y) == Just y" $ \x y ->
        stripPrefix x (x ++ y) == Just (y `asTypeOf` dummy)
    prop "stripPrefix x y == Nothing || x `isPrefixOf` y" $ \x y ->
        stripPrefix x y == Nothing || x `isPrefixOf` (y `asTypeOf` dummy)

main :: IO ()
main = hspec $ do
    describe "dictionary" $ do
        describe "Data.Map" $ dictionaryProps (undefined :: Map Int Char)
        describe "Data.HashMap" $ dictionaryProps (undefined :: HashMap Int Char)
        describe "assoc list" $ dictionaryProps (undefined :: [(Int, Char)])
    describe "map" $ do
        describe "list" $ mapProps (undefined :: [Int]) (+ 1) (+ 2)
        describe "Data.Vector" $ mapProps (undefined :: Vector Int) (+ 1) (+ 2)
        describe "Data.Vector.Unboxed" $ mapProps (undefined :: UVector Int) (+ 1) (+ 2)
        describe "Data.Set" $ mapProps (undefined :: Set Int) (+ 1) (+ 2)
        describe "Data.HashSet" $ mapProps (undefined :: HashSet Int) (+ 1) (+ 2)
        describe "Data.ByteString" $ mapProps (undefined :: ByteString) (+ 1) (+ 2)
        describe "Data.ByteString.Lazy" $ mapProps (undefined :: LByteString) (+ 1) (+ 2)
        describe "Data.Text" $ mapProps (undefined :: Text) succ succ
        describe "Data.Text.Lazy" $ mapProps (undefined :: LText) succ succ
        describe "Data.Sequence" $ mapProps (undefined :: Seq Int) succ succ
    describe "concatMap" $ do
        describe "list" $ concatMapProps (undefined :: [Int]) (\i -> [i + 1, i + 2])
        describe "Data.Vector" $ concatMapProps (undefined :: Vector Int) (\i -> fromList [i + 1, i + 2])
        describe "Data.Vector.Unboxed" $ concatMapProps (undefined :: UVector Int) (\i -> fromList [i + 1, i + 2])
        describe "Data.ByteString" $ concatMapProps (undefined :: ByteString) (\i -> fromList [i + 1, i + 2])
        describe "Data.ByteString.Lazy" $ concatMapProps (undefined :: LByteString) (\i -> fromList [i + 1, i + 2])
        describe "Data.Text" $ concatMapProps (undefined :: Text) (\c -> pack [succ c, succ $ succ c])
        describe "Data.Text.Lazy" $ concatMapProps (undefined :: LText) (\c -> pack [succ c, succ $ succ c])
        describe "Data.Sequence" $ concatMapProps (undefined :: Seq Int) (\i -> pack [i + 1, i + 2])
    describe "filter" $ do
        describe "list" $ filterProps (undefined :: [Int]) (< 20)
        describe "Data.Vector" $ filterProps (undefined :: Vector Int) (< 20)
        describe "Data.Vector.Unboxed" $ filterProps (undefined :: UVector Int) (< 20)
        describe "Data.ByteString" $ filterProps (undefined :: ByteString) (< 20)
        describe "Data.ByteString.Lazy" $ filterProps (undefined :: LByteString) (< 20)
        describe "Data.Text" $ filterProps (undefined :: Text) (< 'A')
        describe "Data.Text.Lazy" $ filterProps (undefined :: LText) (< 'A')
        describe "Data.Map" $ filterProps (undefined :: Map Int Char) (\(i, _) -> i < 20)
        describe "Data.HashMap" $ filterProps (undefined :: HashMap Int Char) (\(i, _) -> i < 20)
        describe "Data.Set" $ filterProps (undefined :: Set Int) (< 20)
        describe "Data.Sequence" $ filterProps (undefined :: Seq Int) (< 20)
    describe "filterM" $ do
        describe "list" $ filterMProps (undefined :: [Int]) (< 20)
        describe "Data.Vector" $ filterMProps (undefined :: Vector Int) (< 20)
        describe "Data.Vector.Unboxed" $ filterMProps (undefined :: Vector Int) (< 20)
        describe "Data.Sequence" $ filterMProps (undefined :: Seq Int) (< 20)
    describe "length" $ do
        describe "list" $ lengthProps (undefined :: [Int])
        describe "Data.Vector" $ lengthProps (undefined :: Vector Int)
        describe "Data.Vector.Unboxed" $ lengthProps (undefined :: UVector Int)
        describe "Data.ByteString" $ lengthProps (undefined :: ByteString)
        describe "Data.ByteString.Lazy" $ lengthProps (undefined :: LByteString)
        describe "Data.Text" $ lengthProps (undefined :: Text)
        describe "Data.Text.Lazy" $ lengthProps (undefined :: LText)
        describe "Data.Map" $ lengthProps (undefined :: Map Int Char)
        describe "Data.HashMap" $ lengthProps (undefined :: HashMap Int Char)
        describe "Data.Set" $ lengthProps (undefined :: Set Int)
        describe "Data.HashSet" $ lengthProps (undefined :: HashSet Int)
        describe "Data.Sequence" $ lengthProps (undefined :: Seq Int)
    describe "mapM" $ do
        describe "list" $ mapMProps (undefined :: [Int])
        describe "Data.Vector" $ mapMProps (undefined :: Vector Int)
        describe "Data.Vector.Unboxed" $ mapMProps (undefined :: UVector Int)
        describe "Seq" $ mapMProps (undefined :: Seq Int)
    describe "mapM_" $ do
        describe "list" $ mapM_Props (undefined :: [Int])
        describe "Data.Vector" $ mapM_Props (undefined :: Vector Int)
        describe "Data.Vector.Unboxed" $ mapM_Props (undefined :: UVector Int)
        describe "Set" $ mapM_Props (undefined :: Set Int)
        describe "HashSet" $ mapM_Props (undefined :: HashSet Int)
        describe "Seq" $ mapM_Props (undefined :: Seq Int)
    describe "fold" $ do
        let f = flip (:)
        describe "list" $ foldProps (undefined :: [Int]) f []
        describe "Data.Vector" $ foldProps (undefined :: Vector Int) f []
        describe "Data.Vector.Unboxed" $ foldProps (undefined :: UVector Int) f []
        describe "Data.ByteString" $ foldProps (undefined :: ByteString) f []
        describe "Data.ByteString.Lazy" $ foldProps (undefined :: LByteString) f []
        describe "Data.Text" $ foldProps (undefined :: Text) f []
        describe "Data.Text.Lazy" $ foldProps (undefined :: LText) f []
        describe "Data.Set" $ foldProps (undefined :: Set Int) f []
        describe "Data.HashSet" $ foldProps (undefined :: HashSet Int) f []
        describe "Data.Sequence" $ foldProps (undefined :: Seq Int) f []
    describe "replicate" $ do
        describe "list" $ replicateProps (undefined :: [Int]) pack
        describe "Data.Vector" $ replicateProps (undefined :: Vector Int) pack
        describe "Data.Vector.Unboxed" $ replicateProps (undefined :: UVector Int) pack
        describe "Data.ByteString" $ replicateProps (undefined :: ByteString) pack
        describe "Data.ByteString.Lazy" $ replicateProps (undefined :: LByteString) pack
        describe "Data.Text" $ replicateProps (undefined :: Text) concat
        describe "Data.Text.Lazy" $ replicateProps (undefined :: LText) concat
        describe "Data.Sequence" $ replicateProps (undefined :: Seq Int) pack
    describe "chunks" $ do
        describe "ByteString" $ chunkProps (asLByteString undefined)
        describe "Text" $ chunkProps (asLText undefined)
    describe "stripSuffix" $ do
        describe "Text" $ stripSuffixProps (undefined :: Text)
        describe "LText" $ stripSuffixProps (undefined :: LText)
        describe "ByteString" $ stripSuffixProps (undefined :: ByteString)
        describe "LByteString" $ stripSuffixProps (undefined :: LByteString)
        describe "Seq" $ stripSuffixProps (undefined :: Seq Int)
    describe "replicateM" $ do
        describe "list" $ replicateMProps (undefined :: [Int])
        describe "Vector" $ replicateMProps (undefined :: Vector Int)
        describe "UVector" $ replicateMProps (undefined :: UVector Int)
        describe "Seq" $ replicateMProps (undefined :: Seq Int)
    describe "encode/decode UTF8" $ do
        describe "Text" $ utf8Props (undefined :: Text)
        describe "LText" $ utf8Props (undefined :: LText)
    describe "compareLength" $ do
        describe "list" $ compareLengthProps (undefined :: [Int])
        describe "Text" $ compareLengthProps (undefined :: Text)
        describe "LText" $ compareLengthProps (undefined :: LText)
    describe "Prefix" $ do
        describe "list" $ prefixProps (undefined :: [Int])
        describe "Text" $ prefixProps (undefined :: Text)
        describe "LText" $ prefixProps (undefined :: LText)
        describe "ByteString" $ prefixProps (undefined :: ByteString)
        describe "LByteString" $ prefixProps (undefined :: LByteString)
        describe "Vector" $ prefixProps (undefined :: Vector Int)
        describe "UVector" $ prefixProps (undefined :: UVector Int)
        describe "Seq" $ prefixProps (undefined :: Seq Int)
    describe "any exceptions" $ do
        it "catchAny" $ do
            failed <- newIORef 0
            tid <- forkIO $ do
                catchAny
                    (threadDelay 20000)
                    (const $ writeIORef failed 1)
                writeIORef failed 2
            threadDelay 10000
            throwTo tid DummyException
            threadDelay 50000
            didFail <- readIORef failed
            liftIO $ didFail `shouldBe` 0
        it "tryAny" $ do
            failed <- newIORef False
            tid <- forkIO $ do
                _ <- tryAny $ threadDelay 20000
                writeIORef failed True
            threadDelay 10000
            throwTo tid DummyException
            threadDelay 50000
            didFail <- readIORef failed
            liftIO $ didFail `shouldBe` False
        it "tryAnyDeep" $ do
            eres <- tryAnyDeep $ return $ throw DummyException
            case eres of
                Left e
                    | Just DummyException <- fromException e -> return ()
                    | otherwise -> error "Expected a DummyException"
                Right () -> error "Expected an exception" :: IO ()

data DummyException = DummyException
    deriving (Show, Typeable)
instance Exception DummyException

instance Arbitrary (Map Int Char) where
    arbitrary = fromList <$> arbitrary
instance Arbitrary (HashMap Int Char) where
    arbitrary = fromList <$> arbitrary
instance Arbitrary (Vector Int) where
    arbitrary = fromList <$> arbitrary
instance Arbitrary (UVector Int) where
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
instance Arbitrary (Seq Int) where
    arbitrary = fromList <$> arbitrary
