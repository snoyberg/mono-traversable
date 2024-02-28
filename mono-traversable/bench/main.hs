{-# LANGUAGE CPP #-}

#if MIN_VERSION_gauge(0,2,0)
import Gauge
#else
import Gauge.Main
#endif

import Sorting (sortingBenchmarks)
import InitTails (initTailsBenchmarks)


main :: IO ()
main = defaultMain
  [ sortingBenchmarks
  , initTailsBenchmarks
  ]
