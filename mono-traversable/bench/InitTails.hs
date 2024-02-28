{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
module InitTails (initTailsBenchmarks) where

#if MIN_VERSION_gauge(0,2,0)
import Gauge
#else
import Gauge.Main
#endif

import Data.Sequences as Ss
import Data.MonoTraversable
import Type.Reflection (Typeable, typeRep)
import Control.DeepSeq
import Data.Foldable (foldl')
import Data.Functor ((<&>))

import Data.ByteString (StrictByteString)
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Data.Sequence (Seq)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

initTailsBenchmarks :: Benchmark
initTailsBenchmarks = bgroup "InitTails"
  [ bmg @[Char]
  , bmg @StrictByteString
  , bmg @LazyByteString
  , bmg @TS.Text
  , bmg @TL.Text
  , bmg @(Seq Char)
  , bmg @(V.Vector  Char)
  , bmg @(VU.Vector Char)
  , bmg @(VS.Vector Char)
  ]

bmg :: forall seq.
  ( TestLabel seq
  , NFData seq
  , IsSequence seq
  , Num (Index seq)
  , Enum (Element seq)
  ) => Benchmark
bmg = bgroup (testLabel @seq) $ bm <$> labelledLengths
  where
  bm :: (String,[Int]) -> Benchmark
  bm (label,lengths) = bgroup label $
    [ ("weak", weakConsume)
    , ("deep", deepConsume)
    ] <&> \(wdLabel,consume) -> bench wdLabel
      $ nf (map $ consume . initTails @seq)
      $ (`Ss.replicate` (toEnum 65)) . fromIntegral <$> lengths
  labelledLengths =
    [ ("tiny",    [0,1,2,5,10])
    , ("small",   [100,150,200,300])
    , ("medium",  [1000,1500,2000,2500])
    , ("large",   [10000,20000,50000])
    , ("extream", [1000000])
    ]

class Typeable a => TestLabel a where
  testLabel :: String
  testLabel = show $ typeRep @a
instance TestLabel [Char]
instance TestLabel StrictByteString where testLabel = "StrictByteString"
instance TestLabel LazyByteString   where testLabel = "LazyByteString"
instance TestLabel TS.Text          where testLabel = "StrictText"
instance TestLabel TL.Text          where testLabel = "LazyText"
instance TestLabel (Seq Char)       where testLabel = "Seq"
instance TestLabel (V.Vector Char)  where testLabel = "Vector"
instance TestLabel (VU.Vector Char) where testLabel = "UnboxedVector"
instance TestLabel (VS.Vector Char) where testLabel = "StorableVector"


-- *Consume used to keep memory usage lower
deepConsume :: NFData seq => [(seq,seq)] -> ()
deepConsume = foldl' (\() (is,ts) -> deepseq is $ deepseq ts ()) ()

weakConsume :: [(seq,seq)] -> ()
weakConsume = foldl' (\() (_,_) -> ()) ()

