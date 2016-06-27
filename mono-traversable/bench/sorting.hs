import Criterion.Main
import Data.Sequences
import Data.MonoTraversable
import qualified Data.List
import qualified System.Random.MWC as MWC
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

asVector :: V.Vector a -> V.Vector a
asVector = id

asUVector :: U.Vector a -> U.Vector a
asUVector = id

main :: IO ()
main = do
    mapM mkGroup [10, 100, 1000, 10000] >>= defaultMain

mkGroup :: Int -> IO Benchmark
mkGroup size = do
    gen <- MWC.create
    inputV <- MWC.uniformVector gen size
    let inputL = otoList (inputV :: V.Vector Int)
        inputVU = fromList inputL :: U.Vector Int
    return $ bgroup (show size)
        [ bench "Data.List.sort" $ nf Data.List.sort inputL
        , bench "list sort" $ nf sort inputL
        , bench "list sort, via vector" $ nf (otoList . sort . asVector . fromList) inputL
        , bench "list sort, via uvector" $ nf (otoList . sort . asUVector . fromList) inputL
        , bench "vector sort" $ nf sort inputV
        , bench "uvector sort" $ nf sort inputVU
        ]
