{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.Sequence
    ( Seq
    ) where

import CorePrelude
import ClassyPrelude.Classes
import qualified Data.Monoid as Monoid
import qualified Control.Monad as Monad
import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (<|), (|>), ViewL(..), viewl, ViewR(..), viewr)


instance CanMap (Seq a) (Seq b) a b where
    map = Monad.fmap

instance CanConcatMap (Seq a) (Seq b) a (Seq b) where
    concatMap = (Monad.=<<)

instance CanFilter (Seq a) a where
    filter = Seq.filter

instance CanFilterM (Seq a) a where
    filterM p =
        Foldable.foldlM
            (\ xs x -> do res <- p x; return $ if res then xs |> x else xs)
            Seq.empty

instance CanLength (Seq a) Int where
    length = Seq.length

instance CanSingleton (Seq a) a where
    singleton = Seq.singleton

instance CanNull (Seq a) where
    null = Seq.null

instance CanPack (Seq a) a where
    pack = Seq.fromList
    unpack = Foldable.toList

instance CanIntersperse (Seq a) a where
    intersperse sep xs = case viewl xs of
        EmptyL -> Seq.empty
        (h :< t) -> h <| prependToAll sep t
      where
        prependToAll sep xs = case viewl xs of
            EmptyL -> Seq.empty
            (h :< t) -> sep <| (h <| prependToAll sep t)

instance Eq a => CanStripPrefix (Seq a) where
    stripPrefix x y
        | x == y1 = Just y2
        | otherwise = Nothing
      where
        (y1, y2) = Seq.splitAt (Seq.length x) y
    isPrefixOf x y = Seq.take (Seq.length x) y == x

instance Eq a => CanStripSuffix (Seq a) where
    stripSuffix x y
        | x == y2 = Just y1
        | otherwise = Nothing
      where
        (y1, y2) = Seq.splitAt (Seq.length y - Seq.length x) y
    isSuffixOf x y = takeRR (Seq.length x) y == x
      where
        takeRR 0 _ = Seq.empty
        takeRR n xs = case viewr xs of
            EmptyR -> Seq.empty
            xs' :> x -> takeRR (n-1) xs' |> x

instance Monad m => CanMapM (Seq i) (m (Seq o)) m i o where
    mapM = Traversable.mapM

instance CanMapM_ (Seq a) a where
    mapM_ = Foldable.mapM_

instance Eq x => CanMember (Seq x) x where
    member x = any (== x)

instance CanBreak (Seq a) a where
    break = Seq.breakl
    span = Seq.spanl
    dropWhile = Seq.dropWhileL
    takeWhile = Seq.takeWhileL

instance CanAny (Seq a) a where
    any = Foldable.any
    all = Foldable.all

instance CanSplitAt (Seq a) Int where
    splitAt = Seq.splitAt

instance CanFold (Seq a) a accum where
    fold = Foldable.foldl'

instance CanReverse (Seq a) where
    reverse = Seq.reverse

instance CanReplicate (Seq a) a Int where
    replicate = Seq.replicate

instance CanReplicateM (Seq a) a Int where
    replicateM = Seq.replicateM

instance CanFind (Seq a) a where
    find p s = case Seq.breakl p s of
        (_, s') -> case Seq.viewl s' of
            a :< _ -> Just a
            _ -> Nothing

instance (Monoid m) => CanConcat (Seq m) m where
    concat = Foldable.fold

instance CanPartition (Seq a) a where
    partition = Seq.partition

instance CanSortBy (Seq a) a where
    sortBy = Seq.sortBy

instance Ord a => CanSort (Seq a) a where
    sort = Seq.sort

instance CanCons (Seq a) a where
    cons = (<|)

instance CanUncons (Seq a) a where
    uncons s = case Seq.viewl s of
        EmptyL -> Nothing
        a :< s' -> Just (a, s')
