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
However, any instance of `EqSequence` now needs to definie `MonoFoldableEq`.


## 0.7.0

* Work on better polymorphic containers
    * Rename `mapKeysWith` to `omapKeysWith`
    * Add new class `BiPolyMap`
    * Add `keys` to `IsSet`
    * New class `HasKeysSet`
* Added `index`, `indexEx` and `unsafeIndex`.
* Added `sortOn`
