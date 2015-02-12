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
