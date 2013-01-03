classy-prelude
==============

A typeclass-based Prelude. For more information, see the blog posts:

* [So many preludes!](http://www.yesodweb.com/blog/2013/01/so-many-preludes) (January 2013)
* [ClassyPrelude: The good, the bad, and the ugly](http://www.yesodweb.com/blog/2012/08/classy-prelude-good-bad-ugly) (August 2012)
* [Clarification: classy prelude](http://www.yesodweb.com/blog/2012/07/clarification-classy-prelude) (July 2012)
* [The classy prelude](http://www.yesodweb.com/blog/2012/07/classy-prelude) (July 2012)

A common question/criticism raised about classy-prelude goes like this: type
classes have to have laws associated with them, and you can't just use
typeclasses to give names to difference things. I'd like to address that
(recurring) comment once, here.

There are many tools and language features (both in Haskell and other
environments) that can be used for purposes other than their originally
intended purposes. Examples include:

* Using OverloadedStrings to express routing (Scotty does this).
* Using a data markup language as a programming language (XSLT).
* Compile-time static assertions using a generic programming technique (C++ and Boost).

The examples are quite numerous. Do I like all of these? No. Many people know
that I happen to utterly despise XSLT. But it's not sufficient to say "it's
using a declarative data format to write code, it's bad." Using a tool for a
different purpose is not really an argument as to why something is bad.

Now there are plenty of reasons you may dislike classy-prelude. I listed some
in [my own blog post](http://www.yesodweb.com/blog/2013/01/so-many-preludes).
And I myself said in that post that a *real* namespacing solution would be
better. But in the interim, this is (AFAIK) the only approach that gives the
same namespacing result, or at least a close approximate. If you don't want to
use it, that's fine. And in fact, that's the whole reason CorePrelude exists:
so that others can experiment with other ideas!

But the statement that "Classes shouldn't be used just to be able to reuse the
same name" is purely a subjective argument, and lacks any kind of explanation
as to why you believe it. I'm happy to hear developed arguments for this, but
such an assertion doesn't really further the discussion at all.

(Also, FWIW, classy-prelude is not the first time that type classes don't have
associated laws. An example straight from base is IsString.)
