{-# LANGUAGE NoMonomorphismRestriction #-}
module Data.Conduit.Combinators.Unqualified
    ( -- * Producers
      -- ** Pure
      yieldMany
    , unfoldC
    , enumFromToC
    , iterateC
    , replicateC
    , sourceLazy

      -- ** Monadic
    , iterMC
    , replicateMC

      -- ** I\/O
    , sourceFile
    , sourceHandle

      -- * Consumers
      -- ** Pure
    , dropC
    , dropCE
    , dropWhileC
    , dropWhileCE
    , foldC
    , foldlC
    , foldCE
    , foldlCE
    , foldMapC
    , foldMapCE
    , allC
    , allCE
    , anyC
    , anyCE
    , andC
    , andCE
    , orC
    , orCE
    , elemC
    , elemCE
    , notElemC
    , notElemCE
    , sinkLazy
    , sinkList
    , sinkVector
    , sinkNull

      -- ** Monadic
    , mapM_C
    , mapM_CE
    , foldMC
    , foldMCE
    , foldMapMC
    , foldMapMCE

      -- ** I\/O
    , sinkFile
    , sinkHandle

      -- * Transformers
      -- ** Pure
    , mapC
    , mapCE
    , omapCE
    , concatMapC
    , concatMapCE
    , takeC
    , takeCE
    , takeWhileC
    , takeWhileCE
    , takeExactlyC
    , takeExactlyCE
    , concatC
    , filterC
    , filterCE
    , mapWhileC

      -- ** Monadic
    , mapMC
    , mapMCE
    , omapMCE
    , concatMapMC
    , filterMC
    , filterMCE
    ) where

import qualified Data.Conduit.Combinators as CC

yieldMany = CC.yieldMany
{-# INLINE yieldMany#-}

unfoldC = CC.unfold
{-# INLINE unfoldC#-}

enumFromToC = CC.enumFromTo
{-# INLINE enumFromToC#-}

iterateC = CC.iterate
{-# INLINE iterateC#-}

replicateC = CC.replicate
{-# INLINE replicateC#-}

sourceLazy = CC.sourceLazy
{-# INLINE sourceLazy#-}

iterMC = CC.iterM
{-# INLINE iterMC#-}

replicateMC = CC.replicateM
{-# INLINE replicateMC#-}

sourceFile = CC.sourceFile
{-# INLINE sourceFile#-}

sourceHandle = CC.sourceHandle
{-# INLINE sourceHandle#-}

dropC = CC.drop
{-# INLINE dropC#-}

dropCE = CC.dropE
{-# INLINE dropCE#-}

dropWhileC = CC.dropWhile
{-# INLINE dropWhileC#-}

dropWhileCE = CC.dropWhileE
{-# INLINE dropWhileCE#-}

foldC = CC.fold
{-# INLINE foldC#-}

foldlC = CC.foldl
{-# INLINE foldlC#-}

foldCE = CC.foldE
{-# INLINE foldCE#-}

foldlCE = CC.foldlE
{-# INLINE foldlCE#-}

foldMapC = CC.foldMap
{-# INLINE foldMapC#-}

foldMapCE = CC.foldMapE
{-# INLINE foldMapCE#-}

allC = CC.all
{-# INLINE allC#-}

allCE = CC.allE
{-# INLINE allCE#-}

anyC = CC.any
{-# INLINE anyC#-}

anyCE = CC.anyE
{-# INLINE anyCE#-}

andC = CC.and
{-# INLINE andC#-}

andCE = CC.andE
{-# INLINE andCE#-}

orC = CC.or
{-# INLINE orC#-}

orCE = CC.orE
{-# INLINE orCE#-}

elemC = CC.elem
{-# INLINE elemC#-}

elemCE = CC.elemE
{-# INLINE elemCE#-}

notElemC = CC.notElem
{-# INLINE notElemC#-}

notElemCE = CC.notElemE
{-# INLINE notElemCE#-}

sinkLazy = CC.sinkLazy
{-# INLINE sinkLazy#-}

sinkList = CC.sinkList
{-# INLINE sinkList#-}

sinkVector = CC.sinkVector
{-# INLINE sinkVector#-}

sinkNull = CC.sinkNull
{-# INLINE sinkNull#-}

mapM_C = CC.mapM_
{-# INLINE mapM_C#-}

mapM_CE = CC.mapM_E
{-# INLINE mapM_CE#-}

foldMC = CC.foldM
{-# INLINE foldMC#-}

foldMCE = CC.foldME
{-# INLINE foldMCE#-}

foldMapMC = CC.foldMapM
{-# INLINE foldMapMC#-}

foldMapMCE = CC.foldMapME
{-# INLINE foldMapMCE#-}

sinkFile = CC.sinkFile
{-# INLINE sinkFile#-}

sinkHandle = CC.sinkHandle
{-# INLINE sinkHandle#-}

mapC = CC.map
{-# INLINE mapC#-}

mapCE = CC.mapE
{-# INLINE mapCE#-}

omapCE = CC.omapE
{-# INLINE omapCE#-}

concatMapC = CC.concatMap
{-# INLINE concatMapC#-}

concatMapCE = CC.concatMapE
{-# INLINE concatMapCE#-}

takeC = CC.take
{-# INLINE takeC#-}

takeCE = CC.takeE
{-# INLINE takeCE#-}

takeWhileC = CC.takeWhile
{-# INLINE takeWhileC#-}

takeWhileCE = CC.takeWhileE
{-# INLINE takeWhileCE#-}

takeExactlyC = CC.takeExactly
{-# INLINE takeExactlyC#-}

takeExactlyCE = CC.takeExactlyE
{-# INLINE takeExactlyCE#-}

concatC = CC.concat
{-# INLINE concatC#-}

filterC = CC.filter
{-# INLINE filterC#-}

filterCE = CC.filterE
{-# INLINE filterCE#-}

mapWhileC = CC.mapWhile
{-# INLINE mapWhileC#-}

mapMC = CC.mapM
{-# INLINE mapMC#-}

mapMCE = CC.mapME
{-# INLINE mapMCE#-}

omapMCE = CC.omapME
{-# INLINE omapMCE#-}

concatMapMC = CC.concatMapM
{-# INLINE concatMapMC#-}

filterMC = CC.filterM
{-# INLINE filterMC#-}

filterMCE = CC.filterME
{-# INLINE filterMCE#-}
