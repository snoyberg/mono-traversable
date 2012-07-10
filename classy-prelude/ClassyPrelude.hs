{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude
    ( -- * Standard
      -- ** Operators
      (Prelude.$)
    , (Prelude.+)
    , (Prelude.-)
    , (Prelude.*)
    , (Prelude./)
    , (Prelude.&&)
    , (Prelude.||)
    , (Prelude..)
      -- ** Functions
    , Prelude.not
    , Prelude.otherwise
    , Prelude.fst
    , Prelude.snd
    , Prelude.id
    , Prelude.maybe
    , Prelude.either
    , Prelude.flip
    , Prelude.const
    , Prelude.error
    , Prelude.zip
    , Prelude.unzip
    , Prelude.zipWith
    , Prelude.or
    , Data.Text.IO.putStrLn
    , Prelude.elem
    , Prelude.odd
    , Prelude.even
    , Prelude.uncurry
      -- ** Type classes
    , Prelude.Ord (..)
    , Prelude.Eq (..)
    , Prelude.Enum (..)
    , Prelude.Show
    , Prelude.Functor (..)
    , Prelude.Monad (..)
    , (Control.Monad.=<<)
      -- ** Data types
    , Prelude.Maybe (..)
    , Prelude.Ordering (..)
    , Prelude.Bool (..)
    , Prelude.Char
    , Prelude.IO
    , Prelude.Either (..)
    , Prelude.Integral (..)
      -- * Re-exports
      -- ** Packed reps
    , ByteString
    , LByteString
    , Text
    , LText
      -- ** Containers
    , Map
    , HashMap
    , LHashMap
    , Set
    , HashSet
    , Vector
      -- ** Numbers
    , Word8
    , Word64
    , Int64
    , Prelude.Int
    , Word
      -- ** Monoids
    , Monoid (..)
    , concat
    , (++)
      -- ** Arrow
    , Control.Arrow.first
    , Control.Arrow.second
    , (Control.Arrow.***)
    , (Control.Arrow.&&&)
      -- ** Maybe
    , Data.Maybe.mapMaybe
    , Data.Maybe.catMaybes
    , Data.Maybe.fromMaybe
      -- ** Either
    , Data.Either.partitionEithers
      -- ** Applicative
    , Control.Applicative.Applicative (..)
    , (Control.Applicative.<$>)
      -- ** Monad
    , (Control.Monad.>=>)
      -- ** Transformers
    , Control.Monad.Trans.Class.lift
    , Control.Monad.IO.Class.MonadIO
    , Control.Monad.IO.Class.liftIO
      -- ** Exceptions
    , Control.Exception.Exception (..)
    , Control.Exception.SomeException
    , Control.Exception.throwIO
      -- ** Files
    , F.FilePath
    , (F.</>)
    , (F.<.>)
    , F.hasExtension
    , F.basename
    , F.filename
      -- * Non-standard
      -- ** List-like classes
    , map
    , concatMap
    , filter
    , length
    , singleton
    , null
    , pack
    , unpack
    , fromList
    , toList
    , mapM
    , mapM_
    , empty
    , stripPrefix
    , break
    , span
    , dropWhile
    , takeWhile
    , any
    , all
    , splitAt
    , fold
      -- ** Map-like
    , lookup
    , insert
    , delete
      -- ** Set-like
    , member
      -- ** Text-like
    , show
      -- ** Files
    , readFile
    , writeFile
      -- ** Print
    , Prelude.print
    ) where

import qualified Prelude
import Prelude (Char, (.))

import ClassyPrelude.Classes
import ClassyPrelude.List ()
import ClassyPrelude.ByteString
import ClassyPrelude.LByteString
import ClassyPrelude.Text
import ClassyPrelude.LText
import ClassyPrelude.Map
import ClassyPrelude.Set
import ClassyPrelude.FilePath ()
import ClassyPrelude.Vector
import ClassyPrelude.HashMap
import ClassyPrelude.LHashMap
import ClassyPrelude.HashSet

import Data.Monoid (Monoid (..))
import qualified Control.Arrow
import qualified Control.Applicative
import qualified Control.Monad
import qualified Control.Exception

import qualified Filesystem.Path.CurrentOS as F

import Data.Word (Word8, Word64, Word)
import Data.Int (Int64)

import qualified Data.Text.IO

import qualified Data.Maybe
import qualified Data.Either

import qualified Control.Monad.Trans.Class
import qualified Control.Monad.IO.Class

concat :: Monoid w => [w] -> w
concat = mconcat

infixr 5  ++
(++) :: Monoid w => w -> w -> w
(++) = mappend

show :: (Prelude.Show a, CanPack c Prelude.Char) => a -> c
show = pack . Prelude.show

fromList :: CanPack c i => [i] -> c
fromList = pack

toList :: CanPack c i => c -> [i]
toList = unpack

-- Misc instances
instance CanPack (Prelude.Maybe a) a where
    pack = Data.Maybe.listToMaybe
    unpack = Data.Maybe.maybeToList
