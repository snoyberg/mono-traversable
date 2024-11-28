# ChangeLog for mono-traversable

## 1.0.21.1

* Enabled native `groupBy` for Vector, U.Vector, VS.Vector since they have been available since vector-0.13.0.0
  [#251](https://github.com/snoyberg/mono-traversable/issues/251)

## 1.0.21.0

* Support for vector 0.13.2.0, adding instances for [`Data.Vector.Strict`](https://hackage.haskell.org/package/vector-0.13.2.0/docs/Data-Vector-Strict.html) data structure.
  [#244](https://github.com/snoyberg/mono-traversable/issues/244)

## 1.0.20.0

* Added instances for [`Reverse`](https://hackage.haskell.org/package/transformers-0.6.1.1/docs/Data-Functor-Reverse.html#t:Reverse) data structure.

## 1.0.19.1

* Removed 'highly experimental' warning haddock comment from Data.Containers.

## 1.0.19.0

* Added `filterWithKey` to `IsMap`.
  [#232](https://github.com/snoyberg/mono-traversable/pull/232)

## 1.0.18.0

* Added MonoPointed instance for text Builder
  [#225](https://github.com/snoyberg/mono-traversable/pull/225)

## 1.0.17.0

* Added `inits`, `tails`, `initTails` to class `IsSequence` with tests and benchmarks for `initTails`.
* Improved ghc benchmark flags.
* Removed extraneous constraint `IsSequence` from `initMay`.

## 1.0.16.0

* Added MonoPointed instance for bytestring Builder
  [#219](https://github.com/snoyberg/mono-traversable/pull/219#pullrequestreview-1879553961)

## 1.0.15.3

* Compile with GHC 9.2 (`Option` removed from `base-4.16`)
  [#199](https://github.com/snoyberg/mono-traversable/issues/199)

## 1.0.15.2

* Support transformers 0.6.0.0 [#196](https://github.com/snoyberg/mono-traversable/issues/196)
* Compile with GHC 9 [#193](https://github.com/snoyberg/mono-traversable/pull/193)

## 1.0.15.1

* Remove whitespace after `@` in as-patterns for GHC HEAD [#186](https://github.com/snoyberg/mono-traversable/pull/186)

## 1.0.15.0

* Added `toNonEmpty` to `Data.NonNull`
  [#185](https://github.com/snoyberg/mono-traversable/pull/185)

## 1.0.14.0
* Added `WrappedMono` to `Data.MonoTraversable`
  [#182](https://github.com/snoyberg/mono-traversable/pull/182)

## 1.0.13.0
* Added `WrappedPoly` to `Data.MonoTraversable`
  [#180](https://github.com/snoyberg/mono-traversable/pull/180)

## 1.0.12.0
* Added `filterSet` to `Data.Containers`
* Use container specific implementations for `filterSet` and `filterMap`
  [#178](https://github.com/snoyberg/mono-traversable/pull/178)

## 1.0.11.0

* Adding monomorphic instances for GHC.Generics and Data.Proxy types
  [#175](https://github.com/snoyberg/mono-traversable/issues/175)

## 1.0.10.0

* Make index work on negative indices
  [#172](https://github.com/snoyberg/mono-traversable/issues/172)
  [#114](https://github.com/snoyberg/mono-traversable/issues/114)

## 1.0.9.0

* Added `filterMap` to `Data.Containers`
  [#167](https://github.com/snoyberg/mono-traversable/pull/167)

## 1.0.8.1

* Compat with gauge 0.1 and 0.2

## 1.0.8.0

* Switch to gauge
* Relax constraint on `singleton` to `MonoPointed`
  [#156](https://github.com/snoyberg/mono-traversable/issues/156)

## 1.0.7.0

* Add `dropPrefix` and `dropSuffix` to `Data.Sequences` [#139](https://github.com/snoyberg/mono-traversable/issues/139)
* Change `sort` implementation [#153](https://github.com/snoyberg/mono-traversable/issues/153)

## 1.0.6.0

* Add `mapNonNull` function to `Data.NonNull` [#150](https://github.com/snoyberg/mono-traversable/issues/150)

## 1.0.5.0

* Move `oelem` and `onotElem` into the `MonoFoldable` class [#133](https://github.com/snoyberg/mono-traversable/issues/133)
    * Change `instance MonoFoldable (Set e)` to `instance Ord e => MonoFoldable (Set e)`

## 1.0.4.0

* Add `dropEnd` function to the `IsSequence` class, and a specialized implementation for `Text`

## 1.0.3.0

* Add `ensurePrefix` and `ensureSuffix` functions [#141](https://github.com/snoyberg/mono-traversable/pull/141)

## 1.0.2.1

* Fix test suite for foldl 1.3

## 1.0.2

* `IsSequence` class: add `lengthIndex` [#127](https://github.com/snoyberg/mono-traversable/pull/127)

## 1.0.1.3

* Make 'olength' for Set and Map O(1) [#125](https://github.com/snoyberg/mono-traversable/pull/125)

## 1.0.1.2

* Support for GHC 8.2

## 1.0.1.1

* Fix typo in rewrite rule

## 1.0.1

* Add `replaceElem` and `replaceSeq` [#107](https://github.com/snoyberg/mono-traversable/pull/107)

## 1.0.0.1

* Add missing export [#101](https://github.com/snoyberg/mono-traversable/pull/101)

## 1.0.0

* Implement the cleanups described in [#95](https://github.com/snoyberg/mono-traversable/issues/95)
    * Split out `Data.MinLen` to `minlen` package, and have `Data.NonNull` stand on its own
    * Remove `Data.ByteVector`
    * Split out extra typeclass instances to `mono-traversable-instances`
* Remove the `Eq` and `Ord` specific classes, and instead use rewrite rules
* Provide the `Data.MonoTraversable.Unprefixed` module
* Generalize `unwords` and `unlines` [#87](https://github.com/snoyberg/mono-traversable/pull/87)
* Add `tailMay` and `initMay` [#89](https://github.com/snoyberg/mono-traversable/issues/89)

## 0.10.2

* Add `delete` and `deleteBy` methods to EqSequence [#94](https://github.com/snoyberg/mono-traversable/pull/94)

## 0.10.1.1

* Remove unneeded INLINEs [#90](https://github.com/snoyberg/mono-traversable/issues/90)

## 0.10.1

* Allow comonad-5 [#86](https://github.com/snoyberg/mono-traversable/issues/86)

## 0.10.0.1

* Instance for Data.Sequence.Seq is incorrect. [#83](https://github.com/snoyberg/mono-traversable/issues/83)

## 0.10.0

* Remove `Functor` instance for `MinLen` [#82](https://github.com/snoyberg/mono-traversable/issues/82)

## 0.9.3

* Added `intercalate`, `splitWhen`, `splitElem`, and `splitSeq` [#80](https://github.com/snoyberg/mono-traversable/pull/80)

## 0.9.2.1

* Tweak test suite for 32-bit systems [#78](https://github.com/snoyberg/mono-traversable/issues/78)

## 0.9.2

* MonoComonad

## 0.9.1

* Fill in missing Mono\* instances [#72](https://github.com/snoyberg/mono-traversable/pull/72)

## 0.9.0.1

* Documentation improvements

## 0.9.0

* Better fixity for mlcons [#56](https://github.com/snoyberg/mono-traversable/issues/56)

## 0.8.0.1

README updates

## 0.8.0

A new MonoFoldableEq class that takes `elem` and `notElem` from `EqSequence`.
`EqSequence` now inherits from `MonoFoldableEq`.

For most users that do not define instances this should not be a breaking change.
However, any instance of `EqSequence` now needs to define `MonoFoldableEq`.


## 0.7.0

* Work on better polymorphic containers
    * Rename `mapKeysWith` to `omapKeysWith`
    * Add new class `BiPolyMap`
    * Add `keys` to `IsSet`
    * New class `HasKeysSet`
* Added `index`, `indexEx` and `unsafeIndex`.
* Added `sortOn`
