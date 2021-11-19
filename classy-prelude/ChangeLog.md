## 1.5.1

* Export a compatiblity shim for `parseTime` as it has been removed in `time-1.10`.
  See <https://hackage.haskell.org/package/time-1.12/changelog>

## 1.5.0

* Removed `alwaysSTM` and `alwaysSucceedsSTM`. See
  <https://github.com/ghc-proposals/ghc-proposals/pull/77>

## 1.4.0

* Switch to `MonadUnliftIO`

## 1.3.1

* Add terminal IO functions

## 1.3.0

* Tracing functions leave warnings when used

## 1.2.0.1

* Use `HasCallStack` in `undefined`

## 1.2.0

* Don't generalize I/O functions to `IOData`, instead specialize to
  `ByteString`. See:
  http://www.snoyman.com/blog/2016/12/beware-of-readfile#real-world-failures

## 1.0.2

* Export `parseTimeM` for `time >= 1.5`

## 1.0.1

* Add the `say` package reexports
* Add the `stm-chans` package reexports

## 1.0.0.2

* Allow basic-prelude 0.6

## 1.0.0.1

* Support for safe-exceptions-0.1.4.0

## 1.0.0

* Support for mono-traversable-1.0.0
* Switch to safe-exceptions
* Add monad-unlift and lifted-async

## 0.12.8

* Add (<&&>),(<||>) [#125](https://github.com/snoyberg/classy-prelude/pull/125)

## 0.12.7

* Concurrency: reexport `Control.Concurrent.Lifted` and provide `yieldThread`

## 0.12.6

* Regeneralize intercalate [#119](https://github.com/snoyberg/classy-prelude/pull/119)
* Add missing exports for `traverse_` and `for_`
* Generalize `mapM_` and `forM_` for GHC 7.10

## 0.12.5.1

* Support for QuickCheck 2.8.2

## 0.12.5

* Expose `Alternative` and `optional`

## 0.12.4

* Expose `traverse_` and `for_`

## 0.12.3

* mono-traversable-0.9.3 support

## 0.12.2

add `errorM`, `terrorM`, and `terror`

## 0.12.0

* Drop system-filepath

## 0.11.1.1

* Compatibility with time >= 1.5 [#100](https://github.com/snoyberg/classy-prelude/pull/100)

## 0.11.1

* Fix deprecation warnings for `elem`

## 0.11.0

* Upgrade to mutable-containers 0.3
* Include dlist support

## 0.10.5

* Export `Data.Mutable`

## 0.10.4

* Expose all of Data.Functor

## 0.10.3

* Expose `liftA` functions and `<**>` [#94](https://github.com/snoyberg/classy-prelude/pull/94)

## 0.10.2

* Provide `foldMap` and `fold` as synonyms for `concatMap` and `concat`.
* Switch to more general `Traversable`-based functions (`sequence_` in particular).
