This package provides common mutable containers, such as double-ended queues
and doubly-linked lists. It is implemented as both an abstract set of type
classes, and concrete implementations.

Note that this library should be considered extremely experimental. That said,
it currently has 100% test coverage and has some performance tuning, though the
API is expected to change significantly.

## Benchmark results

The following benchmarks were performed on January 4, 2015, against version 0.1.1.0.

### Deque benchmark

```
benchmarking IORef [Int]
time                 8.355 ms   (8.350 ms .. 8.362 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 8.353 ms   (8.348 ms .. 8.358 ms)
std dev              15.89 μs   (11.83 μs .. 23.66 μs)

benchmarking IORef (Seq Int)
time                 140.5 μs   (140.4 μs .. 140.6 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 140.5 μs   (140.4 μs .. 140.6 μs)
std dev              313.3 ns   (239.0 ns .. 404.1 ns)

benchmarking UDeque
time                 101.2 μs   (101.2 μs .. 101.2 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 101.2 μs   (101.2 μs .. 101.2 μs)
std dev              16.11 ns   (13.38 ns .. 21.34 ns)

benchmarking SDeque
time                 97.86 μs   (97.85 μs .. 97.88 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 97.88 μs   (97.87 μs .. 97.89 μs)
std dev              38.61 ns   (31.34 ns .. 50.52 ns)

benchmarking BDeque
time                 113.7 μs   (113.7 μs .. 113.7 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 113.7 μs   (113.7 μs .. 113.7 μs)
std dev              29.87 ns   (22.98 ns .. 39.57 ns)

benchmarking DList
time                 160.8 μs   (160.7 μs .. 160.9 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 160.9 μs   (160.8 μs .. 161.0 μs)
std dev              331.8 ns   (277.0 ns .. 401.2 ns)
```

### Ref benchmark

```
benchmarking IORef
time                 4.321 μs   (4.320 μs .. 4.322 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.322 μs   (4.321 μs .. 4.323 μs)
std dev              4.840 ns   (3.746 ns .. 6.242 ns)

benchmarking STRef
time                 4.481 μs   (4.480 μs .. 4.481 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.481 μs   (4.481 μs .. 4.481 μs)
std dev              1.127 ns   (805.5 ps .. 1.758 ns)

benchmarking MutVar
time                 4.478 μs   (4.476 μs .. 4.481 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.479 μs   (4.477 μs .. 4.481 μs)
std dev              6.500 ns   (5.199 ns .. 8.246 ns)

benchmarking URef
time                 2.019 μs   (2.019 μs .. 2.020 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.020 μs   (2.019 μs .. 2.020 μs)
std dev              471.2 ps   (371.2 ps .. 671.9 ps)

benchmarking SRef
time                 2.175 μs   (2.174 μs .. 2.176 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.172 μs   (2.170 μs .. 2.173 μs)
std dev              5.106 ns   (4.054 ns .. 6.660 ns)

benchmarking VRef
time                 4.280 μs   (4.279 μs .. 4.280 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.281 μs   (4.280 μs .. 4.283 μs)
std dev              4.552 ns   (1.911 ns .. 8.892 ns)
```
