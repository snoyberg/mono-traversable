mono-traversable
================

Type classes for mapping, folding, and traversing monomorphic and polymorphic containers.
Haskell is good at operating over polymorphic containers such as a list `[a]`.
A monomorphic container is one such as Text which has a type `Text` that does not expose a type variable for the underlying characters.

mono-traversable also adds

  * `IsSequence`, etc for operating over sequential data types
  * `IsSet`, `IsMap`, etc for unifying set and map APIs
  * `MinLen` for making partial functions (head, tail) total


Standard Type-classess
----------------------

in the upcoming GHC 7.10, using `Functor`, `Foldable`, and `Traversable` will become common-place. This means that rather than using `List.map`, `Vector.map`, etc, the map from the prelude will work on all data types that are a Functor. Of course, you can already do this now using `fmap`.

For a Haskeller, it is important to understand `Functor`, `Applicative`, `Monad`, `Foldable`, and `Monoid`: these are encountered in every day code. For mono-traversable, it is most important to understand [Foldable](https://www.haskell.org/haskellwiki/Typeclassopedia#Foldable).

mono-traversable Type-classess
----------------------

### MonoFunctor

Same as Functor, but cannot change the type.

``` haskell
type family   Element mono
type instance Element Text = Char
type instance Element [a] = a
```

Element is a type family. This tells the compiler to substitute `Char` for `Element Text`.
We can create this rule for every monomorphic container we want to operate on such as `Text`
And we can also create it for a polymorphic container.

Now lets compare MonoFunctor to the normal Functor.

``` haskell
fmap :: Functor f => (a -> b) -> f a -> f b
omap :: MonFunctor mono => (Element mono -> Element mono) -> mono -> mono
```

So there is no type-change from `a` to `b`, the contained type must stay the same (`Element mono -> Element mono`).

Here is the MonoFunctor typeclass definition

``` haskell
class MonoFunctor mono where
    omap :: (Element mono -> Element mono) -> mono -> mono
    default omap :: (Functor f, Element (f a) ~ a, f a ~ mono) => (a -> a) -> f a -> f a
    omap = fmap
```

And we can write some instances

``` haskell
instance MonoFunctor T.Text where
    omap = T.map

instance MonoFunctor [a]
```

The list definition was able to default to using `fmap` so no body was needed.


### MonoFoldable

Same as Foldable, but also operates over monomorphic containers.

MonoFoldable is the heart of the power of mono-traversable (and arguable the package should be named mono-foldable) because anything that can be done with `Foldable` can be done with `MonoFoldable`.
The reason why is that a monomorphic container can never change its type.
So `omap` is a restricted `fmap`.
However, folding generates a *new* structure, so we have no such concerns.
In the classy-prelude package, map is set to `fmap` and omap must be used separately.
However, foldMap is set to just use the mono-traversable version: `ofoldMap`

``` haskell
class Foldable t where
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldr   :: (a -> b -> b) -> b -> t a -> b
  ...

class MonoFoldable mono where
  ofoldMap :: Monoid m => (Element mono -> m) -> mono -> m
  ofoldr :: (Element mono -> b -> b) -> b -> mono -> b
  ...
```

There are additional Typeclasses which build on MonoFoldable

``` haskell
class (MonoFoldable mono, Monoid mono) => MonoFoldableMonoid mono where
    oconcatMap :: (Element mono -> mono) -> mono -> mono

class (MonoFoldable mono, Ord (Element mono)) => MonoFoldableOrd mono where
    maximumEx :: mono -> Element mono
    minimumEx :: mono -> Element mono
```


### MonoTraversable

`Traversable` for monomprohic containers, just as `MonoFunctor` is `Functor` for monomorphic containes.

``` haskell
class (Functor t, Foldable t) => Traversable t where
  traverse  :: Applicative f => (a -> f b) -> t a -> f (t b)
  ...

class (MonoFunctor mono, MonoFoldable mono) => MonoTraversable mono where
  otraverse :: Applicative f => (Element mono -> f (Element mono)) -> mono -> f mono
  ...
```

Adding instances
----------------

If you have a data type which is a member of one of the relevant typeclasses ([Functor](http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-Functor.html),
[Foldable](http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-Foldable.html),
[Traversable](http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-Traversable.html)), its quite easy to add an instance for 
[MonoFunctor](https://hackage.haskell.org/package/mono-traversable-0.2.0.0/docs/Data-MonoTraversable.html#t:MonoFunctor), [MonoFoldable](https://hackage.haskell.org/package/mono-traversable-0.2.0.0/docs/Data-MonoTraversable.html#t:MonoFoldable) or [MonoTraversable](https://hackage.haskell.org/package/mono-traversable-0.2.0.0/docs/Data-MonoTraversable.html#t:MonoTraversable).

You just have to declare the proper ```type instance```:

```Haskell
    {-# LANGUAGE TypeFamilies         #-}
    
    (...)
    
    -- type instance Element T.Text = Char  -- already defined
    -- type instance Element [a] = a        -- here for example
    type instance Element (CustomType a) = a
```

And then, the needed instances:

```Haskell
    instance MonoFunctor (CustomType a)
    instance MonoFoldable (CustomType a)
    instance MonoTraversable (CustomType a)
```
    

in your code, and your ready to use ```CustomType a``` with the functions defined in this package.

**Note**: if your type is as _monomorphic container_ without the proper typeclasses, then you will have to provide an implementation. However, this should be fairly simple, as it can be seen [in the code](https://hackage.haskell.org/package/mono-traversable-0.2.0.0/docs/src/Data-MonoTraversable.html#line-234)


mono-traversable versus lens Traversal
--------------------------------------
lens is a huge package with a lot of functionality.
One piece of functionality it exposes is Fold and Traversal which can also be used to deal with monomorphic containers.

You could prefer mono-traversable to using this part of lens because

* There is really no new API to learn. If you know Foldable, you can use MonoFoldable just as easily
* mono-traversable's typeclass based approach means many methods are included in the class but can easily be given specialised optimized implementations
* You don't need to explicitly pass around the Traversal

The last point is also a point of inflexibility and points to a use case where you could prefer using a lens Traversal.
mono-traversable treats ByteString as a sequence of bytes.
If you want to treat it as both bytes and characters, mono-traversable would require a newtype wrapper around ByteString,
whereas a lens traversal would just use a different traversal function.




[![Build Status](https://secure.travis-ci.org/snoyberg/mono-traversable.png)](http://travis-ci.org/snoyberg/mono-traversable)
