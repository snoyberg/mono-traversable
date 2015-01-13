{-# LANGUAGE TypeFamilies #-}
import Control.Monad             (forM_)
import Data.Mutable
import Data.Sequence             (Seq)
import Data.Vector               (Vector)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

main :: IO ()
main = hspec spec

data RefAction
    = WriteRef Int
    | ModifyRef Int
    | ModifyRef' Int
    | AtomicModifyRef Int
    | AtomicModifyRef' Int
    deriving Show
instance Arbitrary RefAction where
    arbitrary = oneof
        [ fmap WriteRef arbitrary
        , fmap ModifyRef arbitrary
        , fmap ModifyRef' arbitrary
        , fmap AtomicModifyRef arbitrary
        , fmap AtomicModifyRef' arbitrary
        ]

data DequeAction
    = PushFront Int
    | PushBack Int
    | PopFront
    | PopBack
    deriving Show
instance Arbitrary DequeAction where
    arbitrary = oneof $ concat
        [ replicate 25 $ fmap PushFront arbitrary
        , replicate 25 $ fmap PushBack arbitrary
        , [return PopFront, return PopBack]
        ]

specialCase :: [DequeAction]
specialCase =
    [PushBack 9, PushBack 5,PushBack 11,PushBack 2,PushBack 13,PushBack 10,PushBack 4,PushBack 13,PushBack 7,PushBack 8,PushBack 6,PushBack 4,PushBack 7,PushBack 9,PushBack 10,PushBack 3,PushBack 2,PushBack 12,PushBack 12 ,PushBack 6,PushBack 3,PushBack 5,PushBack 14,PushBack 14,PushBack 11,PushBack 8,PopFront,PopFront,PopFront,PushBack 11,PushBack 3,PopFront,PopFront,PushBack 13,PushBack 12,PopFront,PushBack 10,PushBack 7,PopFront,PopFront,PushBack 13,PushBack 9,PopFront,PushBack 7,PushBack 2,PopFront,PopFront,PushBack 6,PushBack 4,PopFront,PopFront,PopFront,PushBack 9,PushBack 3,PopFront,PushBack 10,PushBack 6,PopFront,PopFront,PopFront,PushBack 12,PushBack 5,PopFront,PushBack 12,PushBack 5,PopFront,PushBack 6,PushBack 4,PopFront,PopFront,PopFront,PushBack 14,PushBack 10,PopFront,PushBack 14,PushBack 10,PopFront,PushBack 11,PushBack 8,PopFront,PushBack 8,PushBack 2,PopFront,PopFront,PopFront,PushBack 13,PushBack 7,PopFront,PushBack 12,PushBack 5,PopFront,PushBack 10,PushBack 8, PopFront,PushBack 7,PushBack 2,PopFront,PopFront,PushBack 9,PushBack 4,PopFront,PopFront,PopFront,PopFront,PopFront,PopFront,PopFront,PopFront,PushBack 4,PushBack 9,PushBack 3,PushBack 10,PushBack 6,PushBack 4,PushBack 13,PushBack 7,PushBack 9,PushBack 3,PopFront]

spec :: Spec
spec = do
    describe "Deque" $ do
        let runActions forceType actions = do
                base <- newColl :: IO (IORef [Int])
                tested <- fmap forceType newColl
                forM_ (PopFront : PopBack : actions) $ \action -> do
                    case action of
                        PushFront i -> do
                            pushFront base i
                            pushFront tested i
                        PushBack i -> do
                            pushBack base i
                            pushBack tested i
                        PopFront -> do
                            expected <- popFront base
                            actual <- popFront tested
                            actual `shouldBe` expected
                        PopBack -> do
                            expected <- popBack base
                            actual <- popBack tested
                            actual `shouldBe` expected
                let drain = do
                        expected <- popBack base
                        actual <- popBack tested
                        actual `shouldBe` expected
                        case actual of
                            Just _ -> drain
                            Nothing -> return $! ()
                drain
        let test name forceType = describe name $ do
                prop "arbitrary actions" $ runActions forceType
                it "special case" $ runActions forceType specialCase

        test "UDeque" asUDeque
        test "SDeque" asSDeque
        test "BDeque" asBDeque
        test "DList" asDList
        test "MutVar Seq" (id :: MutVar (PrimState IO) (Seq Int) -> MutVar (PrimState IO) (Seq Int))
        test "STRef Vector" (id :: STRef (PrimState IO) (Vector Int) -> STRef (PrimState IO) (Vector Int))
        test "BRef Vector" (id :: BRef (PrimState IO) (Vector Int) -> BRef (PrimState IO) (Vector Int))
    describe "Ref" $ do
        let test name forceType atomic atomic' = prop name $ \start actions -> do
                base <- fmap asIORef $ newRef start
                tested <- fmap forceType $ newRef start
                let check = do
                        expected <- readRef base
                        actual <- readRef tested
                        expected `shouldBe` actual
                forM_ (actions :: [RefAction]) $ \action -> case action of
                    WriteRef i -> do
                        writeRef base i
                        writeRef tested i
                        check
                    ModifyRef i -> do
                        modifyRef base (+ i)
                        modifyRef tested (+ i)
                        check
                    ModifyRef' i -> do
                        modifyRef' base (subtract i)
                        modifyRef' tested (subtract i)
                        check
                    AtomicModifyRef i -> do
                        let f x = (x + i, ())
                        atomicModifyRef base f
                        _ <- atomic tested f
                        check
                    AtomicModifyRef' i -> do
                        atomicModifyRef' base $ \x -> (x - i, ())
                        _ <- atomic' tested $ \x -> (x - i, ())
                        check
        test "URef" asURef modifyRefHelper modifyRefHelper'
        test "PRef" asPRef modifyRefHelper modifyRefHelper'
        test "SRef" asSRef modifyRefHelper modifyRefHelper'
        test "BRef" asBRef modifyRefHelper modifyRefHelper'
        test "STRef" asSTRef modifyRefHelper modifyRefHelper'
        test "MutVar" asMutVar atomicModifyRef atomicModifyRef'

modifyRefHelper :: (MCState c ~ PrimState IO, RefElement c ~ Int, MutableRef c)
                => c
                -> (Int -> (Int, ()))
                -> IO ()
modifyRefHelper ref f = modifyRef ref $ \i ->
    let (x, y) = f i
     in y `seq` x

modifyRefHelper' :: (MCState c ~ PrimState IO, RefElement c ~ Int, MutableRef c)
                 => c
                 -> (Int -> (Int, ()))
                 -> IO ()
modifyRefHelper' ref f = modifyRef' ref $ \i ->
    let (x, y) = f i
     in y `seq` x
