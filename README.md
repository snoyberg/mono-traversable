## mono-traversable and classy-prelude mega-repo

This repository contains packages in the mono-traversable and classy-prelude
families. Please see the individual READMEs for more details:

You probably want to view [the README for mono-traversable
itself](https://github.com/snoyberg/mono-traversable/tree/master/mono-traversable#readme).

Additional packages in this repository:

* [mono-traversable](https://github.com/snoyberg/mono-traversable/tree/master/mono-traversable#readme)
  providing a set of classes for dealing with monomorphic data structures (like `ByteString` and `Text`)
  in a similar way to how the standard libraries treat polymorphic structures like lists
    * [mono-traversable-instances](https://github.com/snoyberg/mono-traversable/tree/master/mono-traversable-instances#readme),
      containing orphans instances for mono-traversable classes
* [chunked-data](https://github.com/snoyberg/mono-traversable/tree/master/chunked-data#readme),
  providing typeclasses for dealing with various chunked data representations
* [classy-prelude](https://github.com/snoyberg/mono-traversable/tree/master/classy-prelude#readme),
  a Prelude replacement based around the above packages (and many others)
    * [classy-prelude-conduit](https://github.com/snoyberg/mono-traversable/tree/master/classy-prelude-conduit#readme),
      extends classy-prelude with [conduit support](https://github.com/snoyberg/conduit)
    * [classy-prelude-yesod](https://github.com/snoyberg/mono-traversable/tree/master/classy-prelude-yesod#readme),
      extends classy-prelude-conduit with [Yesod web frameworksupport](http://www.yesodweb.com)
* [minlen](https://github.com/snoyberg/mono-traversable/tree/master/minlen#readme),
  provided a newtype wrapper with type-level annotation of minimum container
  length. This is a generalization of the `Data.NonNull` module in `mono-traversable`
