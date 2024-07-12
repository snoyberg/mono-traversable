{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
import Control.Monad
import Gauge.Main
import Data.Mutable

test :: (MCState c ~ PrimState IO, RefElement c ~ Int, MutableRef c)
     => String
     -> (c -> c)
     -> Benchmark
test name forceType = bench name $ whnfIO $ do
    ref <- fmap forceType $ newRef (5 :: Int)
    replicateM_ 500 $ do
        modifyRef' ref (+ 1)
        modifyRef' ref (subtract 1)
        void $ readRef ref
    replicateM_ 500 $ do
        writeRef ref (5 :: Int)
        void $ readRef ref
{-# INLINE test #-}

main :: IO ()
main = defaultMain
    [ test "IORef" asIORef
    , test "STRef" asSTRef
    , test "MutVar" asMutVar
    , test "URef" asURef
    , test "PRef" asPRef
    , test "SRef" asSRef
    , test "BRef" asBRef
    ]
