classy-prelude
==============

A better Prelude. Haskell's Prelude needs to maintain backwards compatibility and has many aspects that no longer represents best practice. The goals of classy-prelude are:

* remove all partial functions
* modernize data structures
  * generally use Text instead of String
  * encourage the use of appropriate data structures such as Vectors or HashMaps instead of always using lists and associated lists
* reduce import lists and the need for qualified imports

classy-prelude [should only be used by application developers](http://www.yesodweb.com/blog/2013/10/prelude-replacements-libraries). Library authors should consider using [mono-traversable](https://github.com/snoyberg/mono-traversable/blob/master/README.md), which classy-prelude builds upon.

It is worth noting that classy-prelude [largely front-ran changes that the community made to the base Prelude in GHC 7.10](http://www.yesodweb.com/blog/2014/10/classy-base-prelude).

mono-traversable
================

Most of this functionality is provided by [mono-traversable](https://github.com/snoyberg/mono-traversable). Please read the README over there. classy-prelude gets rid of the `o` prefix from mono-traversable functions.


Text
====

Lots of things use `Text` instead of `String`.
Note that `show` returns a `String`.
To get back `Text`, use `tshow`.


other functionality
===================

* exceptions package
* system-filepath convenience functions
* whenM, unlessM
* hashNub and ordNub (efficient nub implementations).


Using classy-prelude
====================

* use the NoImplicitPrelude extension (you can place this in your cabal file) and `import ClassyPrelude`
* use [base-noprelude](https://github.com/hvr/base-noprelude) in your project and define a Prelude module that re-exports `ClassyPrelude`.


Appendix
========

* The [mono-traversable](https://github.com/snoyberg/mono-traversable) README.
* [The transition to the modern design of classy-prelude](http://www.yesodweb.com/blog/2013/09/classy-mono).

These blog posts contain some out-dated information but might be helpful
* [So many preludes!](http://www.yesodweb.com/blog/2013/01/so-many-preludes) (January 2013)
* [ClassyPrelude: The good, the bad, and the ugly](http://www.yesodweb.com/blog/2012/08/classy-prelude-good-bad-ugly) (August 2012)



