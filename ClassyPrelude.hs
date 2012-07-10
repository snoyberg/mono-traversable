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
    , Set
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
      -- ** Conduit
    , module Data.Conduit
      -- ** XML
    , X.Document (..)
    , X.Name (..)
    , X.Prologue (..)
    , X.Node (..)
    , X.Element (..)
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

import Data.Monoid (Monoid (..))
import qualified Control.Arrow
import qualified Control.Applicative
import qualified Control.Monad
import qualified Control.Exception

import qualified Filesystem.Path.CurrentOS as F
import qualified Text.XML as X

import Data.Word (Word8, Word64, Word)
import Data.Int (Int64)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO
import qualified Data.Text.Lazy as TL

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.Maybe
import qualified Data.Either

import qualified Control.Monad.Trans.Class
import qualified Control.Monad.IO.Class
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB

type LText = TL.Text

concat :: Monoid w => [w] -> w
concat = mconcat

infixr 5  ++
(++) :: Monoid w => w -> w -> w
(++) = mappend

instance Prelude.Monad m => CanMap (Pipe l i o r m r) i o where
    map = CL.map

instance (co ~ Text, i ~ Char, o ~ Char) => CanMapFunc Text co i o where
    mapFunc = T.map
instance (co ~ LText, i ~ Char, o ~ Char) => CanMapFunc LText co i o where
    mapFunc = TL.map
instance CanMapFunc (Map k v1) (Map k v2) v1 v2 where
    mapFunc = Map.map
instance (Prelude.Ord a, Prelude.Ord b) => CanMapFunc (Set a) (Set b) a b where
    mapFunc = Set.map

instance (co ~ Text, i ~ Char, o ~ Text) => CanConcatMapFunc Text co i o where
    concatMapFunc = T.concatMap
instance (co ~ LText, i ~ Char, o ~ LText) => CanConcatMapFunc LText co i o where
    concatMapFunc = TL.concatMap

instance (Prelude.Monad m, r ~ r') => CanFilter (Pipe l i i r m r') i where
    filter = CL.filter

instance CanFilterFunc Text Char where
    filterFunc = T.filter
instance CanFilterFunc LText Char where
    filterFunc = TL.filter
instance Prelude.Ord k => CanFilterFunc (Map k v) (k, v) where
    filterFunc = Map.filterWithKey . Prelude.curry

instance CanLength Text Prelude.Int where
    length = T.length
instance CanLength (Map k v) Prelude.Int where
    length = Map.size
instance CanLength (Set x) Prelude.Int where
    length = Set.size

instance CanSingleton Text Prelude.Char where
    singleton = T.singleton
instance CanSingleton LText Prelude.Char where
    singleton = TL.singleton
instance (v' ~ v) => CanSingleton (v' -> Map k v) k where
    singleton = Map.singleton
instance CanSingleton (Set x) x where
    singleton = Set.singleton

instance CanNull Text where
    null = T.null
instance CanNull LText where
    null = TL.null
instance CanNull (Map k v) where
    null = Map.null
instance CanNull (Set x) where
    null = Set.null

instance CanPack (Prelude.Maybe a) a where
    pack = Data.Maybe.listToMaybe
    unpack = Data.Maybe.maybeToList
instance CanPack Text Prelude.Char where
    pack = T.pack
    unpack = T.unpack
instance CanPack LText Prelude.Char where
    pack = TL.pack
    unpack = TL.unpack
instance Prelude.Ord k => CanPack (Map k v) (k, v) where
    pack = Map.fromList
    unpack = Map.toList
instance Prelude.Ord x => CanPack (Set x) x where
    pack = Set.fromList
    unpack = Set.toList
instance CanPack F.FilePath Prelude.Char where
    pack = F.decodeString
    unpack = F.encodeString

instance (i ~ i', Prelude.Monad m, m ~ m', u ~ r, o' ~ ()) => CanMapM_ (Pipe l i o u m r) i' o' m' where
    mapM_ = CL.mapM_

instance Prelude.Ord k => CanLookup (Map k v) k v where
    lookup = Map.lookup

instance CanEmpty Text where
    empty = T.empty
instance CanEmpty LText where
    empty = TL.empty
instance CanEmpty (Map k v) where
    empty = Map.empty
instance CanEmpty (Set x) where
    empty = Set.empty

instance (Prelude.Ord x, Set x ~ s, x ~ x') => CanInsert (x' -> s -> Set x) where
    insert = Set.insert

instance Prelude.Ord k => CanInsertVal (Map k v) k v where
    insertVal = Map.insert

instance Prelude.Ord k => CanDelete (Map k v) k where
    delete = Map.delete

instance Prelude.Ord x => CanMember (Set x) x where
    member = Set.member

instance MonadIO m => CanReadFile (m X.Document) where
    readFile = liftIO . X.readFile X.def
instance MonadResource m => CanReadFile (Pipe l i ByteString u m ()) where
    readFile = CB.sourceFile . unpack

instance (u ~ r, MonadResource m) => CanWriteFile (Pipe l ByteString o u m r) where
    writeFile = CB.sinkFile . unpack

instance CanWriteFileFunc X.Document where
    writeFileFunc fp = liftIO . X.writeFile X.def fp

instance CanStripPrefix F.FilePath where
    stripPrefix = F.stripPrefix
instance CanStripPrefix Text where
    stripPrefix = T.stripPrefix
instance CanStripPrefix LText where
    stripPrefix = TL.stripPrefix

instance CanBreak Text Prelude.Char where
    break = T.break
    span = T.span
    dropWhile = T.dropWhile
    takeWhile = T.takeWhile
instance CanBreak LText Prelude.Char where
    break = TL.break
    span = TL.span
    dropWhile = TL.dropWhile
    takeWhile = TL.takeWhile

instance CanAny Text Prelude.Char where
    any = T.any
    all = T.all
instance CanAny LText Prelude.Char where
    any = TL.any
    all = TL.all

instance CanSplitAt Text Prelude.Int where
    splitAt = T.splitAt
instance CanSplitAt LText Int64 where
    splitAt = TL.splitAt

instance (Prelude.Monad m, accum ~ accum') => CanFold accum a (Pipe l a o u m accum') where
    fold = CL.fold

show :: (Prelude.Show a, CanPack c Prelude.Char) => a -> c
show = pack . Prelude.show

fromList :: CanPack c i => [i] -> c
fromList = pack

toList :: CanPack c i => c -> [i]
toList = unpack
