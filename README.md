One of Haskell's strengths is immutable data structures. These structures make
it easier to reason about code, simplify concurrency and parallelism, and in
some cases can improve performance by allowing sharing. However, there are still
classes of problems where mutable data structures can both be more convenient,
and provide a performance boost. This library is meant to provide such
structures in a performant, well tested way. It also provides a simple
abstraction over such data structures via typeclasses.

Before anything else, let me provide the caveats of this package:

* Don't use this package unless you have a good reason to! Immutable data structures are a better approach most of the time!
* This code is intentionally *not* multithread safe. If you need something like a concurrent queue, there are many options on Hackage, from `Chan` to `TChan`, to [chaselev-deque](http://hackage.haskell.org/package/chaselev-deque).

We'll first talk about the general approach to APIs in this package. Next,
there are two main sets of abstractions provided, which we'll cover in the
following two sections, along with their concrete implementations. Finally,
we'll cover benchmarks.

## API structure

The API takes heavy advantage of the `PrimMonad` typeclass from the primitive
package. This allows our data structures to work in both `IO` and `ST` code.
Each data structure has an associated type, `MCState`, which gives the
primitive state for that structure. For example, in the case of `IORef`, that
state is `RealWorld`, whereas for `STRef s`, it would be `s`. This associated
type is quite similar to the `PrimState` associated type from primitive, and in
many type signatures you'll see an equality constraint along the lines of:

```haskell
PrimState m ~ MCState c
```

For those who are wondering, `MCState` stands for "mutable container state."

All actions are part of a typeclass, which allows for generic access to
different types of structures quite easily. In addition, we provide type hint
functions, such as `asIORef`, which can help specify types when using such
generic functions. For example, a common idiom might be:

```haskell
ioref <- fmap asIORef $ newRef someVal
```

Wherever possible, we stick to well accepted naming and type signature
standards. For example, note how closely `modifyRef` and `modifyRef'` match
`modifyIORef` and `modifyIORef'`.

## Single cell references

The base package provides both `IORef` and `STRef` as boxed mutable references,
for storing a single value. The primitive package also provides `MutVar`, which
generalizes over both of those and works for any `PrimMonad` instance. The
`MutableRef` typeclass in this package abstracts over all three of those. It
has two associated types: `MCState` for the primitive state, and `RefElement`
to specify what is contained by the reference.

You may be wondering: why not just take the reference as a type parameter? That
wouldn't allow us to have monomorphic reference types, which may be useful
under some circumstances. This is a similar motivation to how the
`mono-traversable` package works.

In addition to providing an abstraction over `IORef`, `STRef`, and `MutVar`,
this package provides four addition single-cell mutable references. `URef`,
`SRef`, and `BRef` all contain a 1-length mutable vector under the surface,
which is unboxed, storable, and boxed, respectively. The advantage of the first
two over boxed standard boxed references is that it can avoid a significant
amount of allocation overhead. See [the relevant Stack Overflow
discussion](http://stackoverflow.com/questions/27261813/why-is-my-little-stref-int-require-allocating-gigabytes)
and the benchmarks below.

While `BRef` doesn't give this same advantage (since the values are still
boxed), it was trivial to include it along with the other two, and does
actually demonstrate a performance advantage. Unlike `URef` and `SRef`, there
is no restriction on the type of value it can store.

The finally reference type is `PRef`. Unlike the other three mentioned, it
doesn't use vectors at all, but instead drops down directly to a mutable
bytearray to store values. This means it has slightly less overhead (no need to
store the size of the vector), but also restricts the types of things that can
be stored (only instances of `Prim`).

You should benchmark your program to determine the most efficient reference
type, but generally speaking `PRef` will be most performant, followed by `URef`
and `SRef`, and finally `BRef`.

## Collections

Collections allow you to push and pop values to the beginning and end of
themselves. Since different data structures allow different operations, each
operation goes into its own typeclass, appropriately named `MutablePushFront`,
`MutablePushBack`, `MutablePopFront`, and `MutablePopBack`. There is also a
parent typeclass `MutableCollection` which provides:

1. The `CollElement` associated type to indicate what kinds of values are in the collection.
2. The `newColl` function to create a new, empty collection.

The `mono-traversable` package provides a typeclass `IsSequence` which
abstracts over sequence-like things. In particular, it provides operations for
`cons`, `snoc`, `uncons`, and `unsnoc`. Using this abstraction, we can provide
an instance for all of the typeclasses listed above for any mutable reference
containing an instance of `IsSequence`, e.g. `IORef [Int]` or `BRef s (Seq
Double)`.

Note that the performance of some of these combinations is *terrible*. In
particular, `pushBack` or `popBack` on a list requires traversing the entire
list, and any push operations on a `Vector` requires copying the entire
contents of the vector. Caveat emptor! If you *must* use one of these
structures, it's highly recommended to use `Seq`, which gives the best overall
performance.

However, in addition to these instances, this package also provides two
additional data structures: double-ended queues and doubly-linked lists. The
former is based around mutable vectors, and therefore as unboxed (`UDeque`),
storable (`SDeque`), and boxed (`BDeque`) variants. Doubly-linked lists have no
such variety, and are simply `DList`s.

For general purpose queue-like structures, `UDeque` or `SDeque` is likely to
give you best performance. As usual, benchmark your own program to be certain,
and see the benchmark results below.

## Benchmark results

The following benchmarks were performed on January 7, 2015, against version 0.2.0.

### Ref benchmark

```
benchmarking IORef
time                 4.322 μs   (4.322 μs .. 4.323 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.322 μs   (4.322 μs .. 4.323 μs)
std dev              1.401 ns   (1.114 ns .. 1.802 ns)

benchmarking STRef
time                 4.484 μs   (4.484 μs .. 4.485 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.484 μs   (4.484 μs .. 4.484 μs)
std dev              941.0 ps   (748.5 ps .. 1.164 ns)

benchmarking MutVar
time                 4.482 μs   (4.482 μs .. 4.483 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.482 μs   (4.482 μs .. 4.483 μs)
std dev              843.2 ps   (707.9 ps .. 1.003 ns)

benchmarking URef
time                 2.020 μs   (2.019 μs .. 2.020 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.020 μs   (2.019 μs .. 2.020 μs)
std dev              955.2 ps   (592.2 ps .. 1.421 ns)

benchmarking PRef
time                 2.015 μs   (2.014 μs .. 2.015 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.014 μs   (2.014 μs .. 2.015 μs)
std dev              901.3 ps   (562.8 ps .. 1.238 ns)

benchmarking SRef
time                 2.231 μs   (2.230 μs .. 2.232 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.231 μs   (2.230 μs .. 2.231 μs)
std dev              1.938 ns   (1.589 ns .. 2.395 ns)

benchmarking BRef
time                 4.279 μs   (4.279 μs .. 4.279 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.279 μs   (4.279 μs .. 4.279 μs)
std dev              1.281 ns   (1.016 ns .. 1.653 ns)
```

### Deque benchmark

```
time                 8.371 ms   (8.362 ms .. 8.382 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 8.386 ms   (8.378 ms .. 8.398 ms)
std dev              29.25 μs   (20.73 μs .. 42.47 μs)

benchmarking IORef (Seq Int)
time                 142.9 μs   (142.7 μs .. 143.1 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 142.7 μs   (142.6 μs .. 142.9 μs)
std dev              542.8 ns   (426.5 ns .. 697.0 ns)

benchmarking UDeque
time                 107.5 μs   (107.4 μs .. 107.6 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 107.5 μs   (107.4 μs .. 107.6 μs)
std dev              227.4 ns   (171.8 ns .. 297.8 ns)

benchmarking SDeque
time                 97.82 μs   (97.76 μs .. 97.89 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 97.82 μs   (97.78 μs .. 97.89 μs)
std dev              169.5 ns   (110.6 ns .. 274.5 ns)

benchmarking BDeque
time                 113.5 μs   (113.4 μs .. 113.6 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 113.6 μs   (113.5 μs .. 113.7 μs)
std dev              300.4 ns   (221.8 ns .. 424.1 ns)

benchmarking DList
time                 156.5 μs   (156.3 μs .. 156.6 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 156.4 μs   (156.3 μs .. 156.6 μs)
std dev              389.5 ns   (318.3 ns .. 502.8 ns)
```

## Test coverage

As of version 0.2.0, this package has 100% test coverage. If you look at the
report yourself, you'll see some uncovered code; it's just the automatically
derived `Show` instance needed for QuickCheck inside the test suite itself.
