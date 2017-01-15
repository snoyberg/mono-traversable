# 1.1.0

* Don't generalize I/O functions to `IOData`, instead specialize to
  `ByteString`. See:
  http://www.snoyman.com/blog/2016/12/beware-of-readfile#real-world-failures

# 1.0.8.3

* Fix version bounds for chunked-data/mono-traversable combos

# 1.0.8.2

* Fix foldl1 not being "a strict left fold" as advertised.
  [#115](https://github.com/snoyberg/mono-traversable/pull/115)

# 1.0.8.1

* Break on single elements and defer monoid concatenation until yield
  [#111](https://github.com/snoyberg/mono-traversable/pull/111)

# 1.0.8

* Add lower bound on conduit 1.2.8 (make it easier to follow [the
  reskin](http://www.snoyman.com/blog/2016/09/proposed-conduit-reskin).

# 1.0.7

* Add `sourceFileBS` and `sinkFileBS`

# 1.0.6

* Add `peekForeverE` combinator

# 1.0.5

* Add head, headDef and lastDef combinators

# 1.0.4

* Move into mono-traversable repo, support mono-traversable 1.0

# 1.0.3.1

* Support for QuickCheck 2.8.2

# 1.0.3

* sourceRandomWith [#19](https://github.com/fpco/conduit-combinators/pull/19)

# 1.0.2

* Make mapAccumWhile & mapAccumS strict in accumulator state [#18](https://github.com/fpco/conduit-combinators/pull/18)

# 1.0.1

* mapAccumWhile, mapAccumWhileM, mapAccumS

# 1.0.0

* Drop system-filepath/system-fileio

# 0.3.1

* `peekForever`

# 0.3.0

Stream fusion enabled, drop compatibility with older conduit
