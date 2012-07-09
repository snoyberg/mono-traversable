{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
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
    , CanMap (..)
    , CanConcatMap (..)
    , CanFilter (..)
    , CanFilterFunc (..)
    , CanLength (..)
    , CanSingleton (..)
    , CanNull (..)
    , CanPack (..)
    , CanMapM (..)
    , CanMapM_ (..)
    , CanEmpty (..)
    , CanStripPrefix (..)
    , CanBreak (..)
    , CanAny (..)
    , CanSplitAt (..)
    , CanFold (..)
    , CanFoldFunc (..)
      -- ** Map-like
    , CanLookup (..)
    , CanInsert (..)
    , CanInsertVal (..)
    , CanDelete (..)
      -- ** Set-like
    , CanMember (..)
      -- ** Text-like
    , CanShow (..)
      -- ** Files
    , CanReadFile (..)
    , CanWriteFile (..)
    , CanWriteFileFunc (..)
      -- ** print
    , Prelude.print
    ) where

import qualified Prelude
import Prelude (Char, (.))
import Data.Monoid (Monoid (..))
import qualified Control.Arrow
import qualified Control.Applicative
import qualified Control.Monad
import qualified Control.Exception

import qualified Filesystem.Path.CurrentOS as F
import qualified Text.XML as X

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
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

import qualified Data.List

type LByteString = L.ByteString
type LText = TL.Text

concat :: Monoid w => [w] -> w
concat = mconcat

infixr 5  ++
(++) :: Monoid w => w -> w -> w
(++) = mappend

class CanMap ci co i o where
    map :: (i -> o) -> ci -> co
instance (i ~ a, co ~ [b]) => CanMap [a] co i b where
    map = Prelude.map
instance (co ~ ByteString, i ~ Word8, o ~ Word8) => CanMap ByteString co i o where
    map = S.map
instance (co ~ LByteString, i ~ Word8, o ~ Word8) => CanMap LByteString co i o where
    map = L.map
instance (co ~ Text, i ~ Char, o ~ Char) => CanMap Text co i o where
    map = T.map
instance (co ~ LText, i ~ Char, o ~ Char) => CanMap LText co i o where
    map = TL.map
instance CanMap (Map k v1) (Map k v2) v1 v2 where
    map = Map.map
instance (Prelude.Ord a, Prelude.Ord b) => CanMap (Set a) (Set b) a b where
    map = Set.map

class CanConcatMap ci co i o where
    concatMap :: (i -> o) -> ci -> co
instance (i ~ a, co ~ [b]) => CanConcatMap [a] co i [b] where
    concatMap = Prelude.concatMap
instance (co ~ ByteString, i ~ Word8, o ~ ByteString) => CanConcatMap ByteString co i o where
    concatMap = S.concatMap
instance (co ~ LByteString, i ~ Word8, o ~ LByteString) => CanConcatMap LByteString co i o where
    concatMap = L.concatMap
instance (co ~ Text, i ~ Char, o ~ Text) => CanConcatMap Text co i o where
    concatMap = T.concatMap
instance (co ~ LText, i ~ Char, o ~ LText) => CanConcatMap LText co i o where
    concatMap = TL.concatMap

class CanFilter f where
    filter :: f
instance (b ~ c, CanFilterFunc b a) => CanFilter ((a -> Prelude.Bool) -> b -> c) where
    filter = filterFunc
instance (Prelude.Monad m, r ~ r') => CanFilter ((i -> Prelude.Bool) -> Pipe l i i r m r') where
    filter = CL.filter

class CanFilterFunc c i | c -> i where
    filterFunc :: (i -> Prelude.Bool) -> c -> c
instance CanFilterFunc [a] a where
    filterFunc = Prelude.filter
instance CanFilterFunc ByteString Word8 where
    filterFunc = S.filter
instance CanFilterFunc LByteString Word8 where
    filterFunc = L.filter
instance CanFilterFunc Text Char where
    filterFunc = T.filter
instance CanFilterFunc LText Char where
    filterFunc = TL.filter
instance Prelude.Ord k => CanFilterFunc (Map k v) (k, v) where
    filterFunc = Map.filterWithKey . Prelude.curry

class CanLength c i | c -> i where
    length :: c -> i
instance CanLength [a] Prelude.Int where
    length = Prelude.length
instance CanLength ByteString Prelude.Int where
    length = S.length
instance CanLength LByteString Int64 where
    length = L.length
instance CanLength Text Prelude.Int where
    length = T.length
instance CanLength (Map k v) Prelude.Int where
    length = Map.size
instance CanLength (Set x) Prelude.Int where
    length = Set.size

class CanSingleton c i | c -> i where
    singleton :: i -> c
instance CanSingleton [a] a where
    singleton = Prelude.return
instance CanSingleton ByteString Word8 where
    singleton = S.singleton
instance CanSingleton LByteString Word8 where
    singleton = L.singleton
instance CanSingleton Text Prelude.Char where
    singleton = T.singleton
instance CanSingleton LText Prelude.Char where
    singleton = TL.singleton
instance (v' ~ v) => CanSingleton (v' -> Map k v) k where
    singleton = Map.singleton
instance CanSingleton (Set x) x where
    singleton = Set.singleton

class CanNull c where
    null :: c -> Prelude.Bool
instance CanNull [a] where
    null = Prelude.null
instance CanNull ByteString where
    null = S.null
instance CanNull LByteString where
    null = L.null
instance CanNull Text where
    null = T.null
instance CanNull LText where
    null = TL.null
instance CanNull (Map k v) where
    null = Map.null
instance CanNull (Set x) where
    null = Set.null

class CanPack c i | c -> i where
    pack :: [i] -> c
    unpack :: c -> [i]
instance CanPack [a] a where
    pack = Prelude.id
    unpack = Prelude.id
instance CanPack (Prelude.Maybe a) a where
    pack = Data.Maybe.listToMaybe
    unpack = Data.Maybe.maybeToList
instance CanPack ByteString Word8 where
    pack = S.pack
    unpack = S.unpack
instance CanPack LByteString Word8 where
    pack = L.pack
    unpack = L.unpack
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

class CanMapM output input where
    mapM :: input -> output
instance Prelude.Monad m => CanMapM ([a] -> m [b]) (a -> m b) where
    mapM = Prelude.mapM

class CanMapM_ output input where
    mapM_ :: input -> output
instance (x ~ (), Prelude.Monad m) => CanMapM_ ([a] -> m x) (a -> m b) where
    mapM_ = Prelude.mapM_
instance (i ~ i', Prelude.Monad m, m ~ m', u ~ r) => CanMapM_ (Pipe l i o u m r) (i' -> m' ()) where
    mapM_ = CL.mapM_

class CanLookup c k v | c -> k v where
    lookup :: k -> c -> Prelude.Maybe v
instance Prelude.Eq k => CanLookup [(k, v)] k v where
    lookup = Prelude.lookup
instance Prelude.Ord k => CanLookup (Map k v) k v where
    lookup = Map.lookup

class CanShow t where
    show :: Prelude.Show a => a -> t
instance CanShow Prelude.String where
    show = Prelude.show
instance CanShow Text where
    show = T.pack . Prelude.show
instance CanShow LText where
    show = TL.pack . Prelude.show

class CanEmpty c where
    empty :: c
instance CanEmpty [a] where
    empty = []
instance CanEmpty ByteString where
    empty = S.empty
instance CanEmpty LByteString where
    empty = L.empty
instance CanEmpty Text where
    empty = T.empty
instance CanEmpty LText where
    empty = TL.empty
instance CanEmpty (Map k v) where
    empty = Map.empty
instance CanEmpty (Set x) where
    empty = Set.empty

class CanInsert f where
    insert :: f
instance (CanInsertVal c' k v, c ~ c') => CanInsert (k -> v -> c -> c') where
    insert = insertVal
instance (Prelude.Ord x, Set x ~ s, x ~ x') => CanInsert (x' -> s -> Set x) where
    insert = Set.insert

class CanInsertVal c k v | c -> k v where
    insertVal :: k -> v -> c -> c
instance Prelude.Eq k => CanInsertVal [(k, v)] k v where
    insertVal k v c = (k, v) : delete k c
instance Prelude.Ord k => CanInsertVal (Map k v) k v where
    insertVal = Map.insert

class CanDelete c k | c -> k where
    delete :: k -> c -> c
instance Prelude.Eq k => CanDelete [(k, v)] k where
    delete k = filter ((Prelude./= k) . Prelude.fst)
instance Prelude.Ord k => CanDelete (Map k v) k where
    delete = Map.delete

class CanMember c k | c -> k where
    member :: k -> c -> Prelude.Bool
instance Prelude.Eq x => CanMember [x] x where
    member x = Prelude.any (Prelude.== x)
instance Prelude.Ord x => CanMember (Set x) x where
    member = Set.member

class CanReadFile a where
    readFile :: F.FilePath -> a
instance MonadIO m => CanReadFile (m ByteString) where
    readFile = liftIO . S.readFile . F.encodeString
instance MonadIO m => CanReadFile (m LByteString) where
    readFile = liftIO . L.readFile . F.encodeString
instance MonadIO m => CanReadFile (m X.Document) where
    readFile = liftIO . X.readFile X.def
instance MonadResource m => CanReadFile (Pipe l i ByteString u m ()) where
    readFile = CB.sourceFile . unpack

class CanWriteFile a where
    writeFile :: F.FilePath -> a
instance (MonadIO m, b ~ (), CanWriteFileFunc a) => CanWriteFile (a -> m b) where
    writeFile = writeFileFunc
instance (u ~ r, MonadResource m) => CanWriteFile (Pipe l ByteString o u m r) where
    writeFile = CB.sinkFile . unpack

class CanWriteFileFunc a where
    writeFileFunc :: MonadIO m => F.FilePath -> a -> m ()
instance CanWriteFileFunc ByteString where
    writeFileFunc fp = liftIO . S.writeFile (F.encodeString fp)
instance CanWriteFileFunc LByteString where
    writeFileFunc fp = liftIO . L.writeFile (F.encodeString fp)
instance CanWriteFileFunc X.Document where
    writeFileFunc fp = liftIO . X.writeFile X.def fp

class CanStripPrefix a where
    stripPrefix :: a -> a -> Prelude.Maybe a
instance Prelude.Eq a => CanStripPrefix [a] where
    stripPrefix = Data.List.stripPrefix
instance CanStripPrefix F.FilePath where
    stripPrefix = F.stripPrefix
instance CanStripPrefix Text where
    stripPrefix = T.stripPrefix
instance CanStripPrefix LText where
    stripPrefix = TL.stripPrefix

class CanBreak c i | c -> i where
    break :: (i -> Prelude.Bool) -> c -> (c, c)
    span :: (i -> Prelude.Bool) -> c -> (c, c)
    dropWhile :: (i -> Prelude.Bool) -> c -> c
    takeWhile :: (i -> Prelude.Bool) -> c -> c
instance CanBreak [a] a where
    break = Prelude.break
    span = Prelude.span
    dropWhile = Prelude.dropWhile
    takeWhile = Prelude.takeWhile
instance CanBreak ByteString Word8 where
    break = S.break
    span = S.span
    dropWhile = S.dropWhile
    takeWhile = S.takeWhile
instance CanBreak LByteString Word8 where
    break = L.break
    span = L.span
    dropWhile = L.dropWhile
    takeWhile = L.takeWhile
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

class CanAny c i | c -> i where
    any :: (i -> Prelude.Bool) -> c -> Prelude.Bool
    all :: (i -> Prelude.Bool) -> c -> Prelude.Bool
instance CanAny [a] a where
    any = Prelude.any
    all = Prelude.all
instance CanAny ByteString Word8 where
    any = S.any
    all = S.all
instance CanAny LByteString Word8 where
    any = L.any
    all = L.all
instance CanAny Text Prelude.Char where
    any = T.any
    all = T.all
instance CanAny LText Prelude.Char where
    any = TL.any
    all = TL.all

class CanSplitAt c i | c -> i where
    splitAt :: i -> c -> (c, c)
instance CanSplitAt [c] Prelude.Int where
    splitAt = Prelude.splitAt
instance CanSplitAt ByteString Prelude.Int where
    splitAt = S.splitAt
instance CanSplitAt LByteString Int64 where
    splitAt = L.splitAt
instance CanSplitAt Text Prelude.Int where
    splitAt = T.splitAt
instance CanSplitAt LText Int64 where
    splitAt = TL.splitAt

-- | Strict left fold.
class CanFold accum a f where
    fold :: (accum -> a -> accum) -> accum -> f
instance (accum ~ accum', CanFoldFunc c accum a) => CanFold accum a (c -> accum') where
    fold = foldFunc
instance (Prelude.Monad m, accum ~ accum') => CanFold accum a (Pipe l a o u m accum') where
    fold = CL.fold

class CanFoldFunc c accum a | c -> a where
    foldFunc :: (accum -> a -> accum) -> accum -> c -> accum
instance CanFoldFunc [a] accum a where
    foldFunc = Data.List.foldl'
