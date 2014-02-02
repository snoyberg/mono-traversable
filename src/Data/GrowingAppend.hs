module Data.GrowingAppend where

import Data.MonoTraversable
import Data.Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as VS
import Data.Vector.Instances ()
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import qualified Data.Set as Set
import qualified Data.HashSet as HashSet
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap

-- | olength (x <> y) >= olength x + olength y
class (Semigroup mono, MonoFoldable mono) => GrowingAppend mono

instance GrowingAppend (Seq.Seq a)
instance GrowingAppend [a]
instance GrowingAppend (V.Vector a)
instance U.Unbox a => GrowingAppend (U.Vector a)
instance VS.Storable a => GrowingAppend (VS.Vector a)
instance GrowingAppend S.ByteString
instance GrowingAppend L.ByteString
instance GrowingAppend T.Text
instance GrowingAppend TL.Text
instance GrowingAppend (NE.NonEmpty a)
instance Ord k => GrowingAppend (Map.Map k v)
instance (Eq k, Hashable k) => GrowingAppend (HashMap.HashMap k v)
instance Ord v => GrowingAppend (Set.Set v)
instance (Eq v, Hashable v) => GrowingAppend (HashSet.HashSet v)
instance GrowingAppend IntSet.IntSet
instance GrowingAppend (IntMap.IntMap v)
