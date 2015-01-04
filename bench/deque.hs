import Criterion.Main
import Data.Mutable.Class
import Control.Monad
import Data.IORef (IORef)
import Data.Sequence (Seq)
import Data.Mutable.Deque
import Data.Mutable.DList

test name forceType = bench name $ whnfIO $ do
    let x = 5 :: Int
    coll <- fmap forceType newColl
    replicateM_ 500 $ pushFront coll x
    replicateM_ 500 $ pushBack coll x
    replicateM_ 200 $ void $ popFront coll
    replicateM_ 200 $ void $ popBack coll
    replicateM_ 500 $ do
        pushBack coll x
        pushFront coll x
        void $ popFront coll
    replicateM_ 500 $ do
        pushBack coll x
        pushFront coll x
    replicateM_ 500 $ do
        pushBack coll x
        void $ popFront coll
{-# INLINE test #-}

main :: IO ()
main = defaultMain
    [ test "IORef [Int]" (id :: IORef [Int] -> IORef [Int])
    , test "IORef (Seq Int)" (id :: IORef (Seq Int) -> IORef (Seq Int))
    , test "UDeque" asUDeque
    , test "SDeque" asSDeque
    , test "BDeque" asBDeque
    , test "DList" asDList
    ]
