## 0.3.3

* Move into mono-traversable repo

## 0.3.2.1

* Fix benchmark

## 0.3.2

* Export IOPRef, IOSRef, IOBRef [#5](https://github.com/fpco/mutable-containers/pull/5)

## 0.3.1

* Export IOURef [#4](https://github.com/fpco/mutable-containers/pull/4)

## 0.3.0

* Rename `DList` to `DLList` to avoid conflict with difference lists

## 0.2.1.2

* `Deque` optimizations by avoiding modulus operations completely.

## 0.2.1.1

* Fix a bug in `Deque`'s new vector allocation/copy code.

## 0.2.1

* Export `Prim`, `Unbox` and `Storable`
* `SRef` uses `ForeignPtr` directly (slightly more efficient)

## 0.2.0

* Restructure under the Data.Mutable module.

## 0.1.2.0

* Added PRef

## 0.1.1.0

* Added reference benchmark.
* Added boxed deque and references.
