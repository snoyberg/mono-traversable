mono-traversable
================

Type classes for mapping, folding, and traversing monomorphic and polymorphic containers.
Haskell is good at operating over polymorphic containers such as a list `[a]`.
A monomorphic container is one such as Text which has a type `Text` that does not expose a type variable for the underlying characters.

mono-traversable also adds

  * `IsSequence`, etc for operating over sequential data types
  * `IsSet`, `IsMap`, etc for unifying set and map APIs
  * `MinLen` for making partial functions (head, tail) total


Using Typeclasses
-----------------

There are 2 use cases for mono-traversable: application authors and library authors.

### Library authors

As a library author, if you want to allow a user to pass in a `Text` or a `String`,
then you need to expose an API with a mono-traversable typeclass.
You should think twice about using mono-traversable though because

* Using Typeclasses makes type inference more difficult. It is usually better to force the user to give a `Text`. Another option is to just have multiple APIs.
* If you are operating on polymorphic structures in which the normal typeclasses suffice, you should just use them from base. On the other hand, even if you are using polymorphic containers you may want to leverage `IsSequence` or `MinLen`.

### Application authors

As an application author, you should consider using classy-prelude, which leans heavily on mono-traversable.

When writing your own function signatures, you should default to making them concrete: if you are actually using a list, then make your function take a list rather than an `IsSequence`. This will improve type inference, error messages, and make your code easier to understand. When you decide to use a `Vector` instead of a list, change the type signature to use a `Vector`. When you actually need a function to both accept a `Vector` and a list, it is easy to change the function signature to the more abstract typeclasses that you require.


Standard Typeclasses
--------------------

in the upcoming GHC 7.10, using `Functor`, `Foldable`, and `Traversable` will become common-place. This means that rather than using `List.map`, `Vector.map`, etc, the map from the prelude will work on all data types that are a Functor. Of course, you can already do this now using `fmap`.

For a Haskeller, it is important to understand `Functor`, `Applicative`, `Monad`, `Foldable`, and `Monoid`: these are encountered in every day code. For mono-traversable, it is most important to understand [Foldable](https://www.haskell.org/haskellwiki/Typeclassopedia#Foldable).

mono-traversable Typeclasses
----------------------------

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
omap :: MonoFunctor mono => (Element mono -> Element mono) -> mono -> mono
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

MonoFoldable is the heart of the power of mono-traversable (and arguably the package should be named mono-foldable) because anything that can be done with `Foldable` can be done with `MonoFoldable`.
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

class MonoPointed mono where
    opoint :: Element mono -> mono
```

MonoPointed abstracts over the concept of a singleton. For any `Applicative`, `opoint` is the same as `pure` from Applicative. Since mono-traversable did not bother with a `MonoApplicative` typeclass, we added `MonoPointed` to still have the functionality of `pure`.


### MonoTraversable

`MonoTraversable` is `Traversable` for monomorphic containers, just as
`MonoFunctor` is `Functor` for monomorphic containers.

``` haskell
class (Functor t, Foldable t) => Traversable t where
  traverse  :: Applicative f => (a -> f b) -> t a -> f (t b)
  ...

class (MonoFunctor mono, MonoFoldable mono) => MonoTraversable mono where
  otraverse :: Applicative f => (Element mono -> f (Element mono)) -> mono -> f mono
  ...
```


### Containers

* SetContainer: unifies operations across `Set` and `Map`
* PolyMap: differenceMap and intersectionMap
* IsSet: unifies operations across different `Set`s
* IsMap: unifies operations across different `Map`s
* MonoZip: zip operations on MonoFunctors.

Note that because `Set` and `Map` are not a Functor (and therefore not MonoFoldable), one must use `mapFromList`, `mapToList`, `setFromList`, and `setToList`.


### Sequences

`IsSequence` contains list-like operations.

``` haskell
-- | Sequence Laws:
--
-- > fromList . otoList = id
-- > fromList (x <> y) = fromList x <> fromList y
-- > otoList (fromList x <> fromList y) = x <> y
class (Monoid seq, MonoTraversable seq, SemiSequence seq, MonoPointed seq) => IsSequence seq where
    fromList :: [Element seq] -> seq
    break :: (Element seq -> Bool) -> seq -> (seq, seq)
    ...
```

The laws state that an IsSequence is a list-like (sequential) structure.

* an `IsSequence` is not just something that can be converted to a list (`MonoFoldable`), but something that can be created from a list.
* Converting to and from a list does not change the `IsSequence`, and it doesn't even change the `IsSequence` if you do the conversions on chunks of the `IsSequence`.

SemiSequence is required by IsSequence. It is conceptually the same as IsSequence, but contains operations that can also be used on a `NonEmpty` or a `MinLen` (which are SemiGroups) because they do not reduce the number of elements in the sequence.


There are some more typeclasess that build on top of IsSequence.

``` haskell
class (IsSequence seq, Eq (Element seq)) => EqSequence seq where
class (EqSequence seq, MonoFoldableOrd seq) => OrdSequence seq where
class (IsSequence t, IsString t, Element t ~ Char) => Textual t where
    words :: t -> [t]
    unwords :: [t] -> t
    lines :: t -> [t]
    unlines :: [t] -> t
    toLower :: t -> t
    toUpper :: t -> t
    ...
```

Textual functions are always safe to use with Unicode (it is possible to misuse other functions that operate on individual characters).


### MinLen

Did you notice minimumEx and maximumEx from above? Ex stands for 'Exception'.
An exception will occur if you call minimumEx on an empty list.
MinLen is a tool to guarantee that this never occurs, and instead to prove that there is one or more elements in your list.

``` haskell
minimumEx :: MonoFoldable mono => mono -> Element mono

-- | like Data.List, but not partial on a MonoFoldable
minimum :: MonoFoldableOrd mono => MinLen (Succ nat) mono -> Element mono
minimum = minimumEx . unMinLen

newtype MinLen nat mono = MinLen { unMinLen :: mono }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Functor)

-- Type level naturals
data Zero = Zero
data Succ nat = Succ nat
```

The `minimum` function exposed from `MinLen` is very similar to `minimumEx`, but has a `MinLen` wrapper that ensures it will never throw an exception.
`MinLen` is a newtype with a phantom type that contains information about the minimum number of elements we know are in the structure. That is done through type-level Peano numbers.

What do we know about the input to minimum? If nat is Zero, then it reduces to `MinLen (Succ Zero) mono`. Succ means successor, and the successor of 0 is 1, so the data structure has a minimum length of 1.

Lets see this in practice

``` haskell
> minimum []
<interactive>:3:9:
    Couldn't match expected type ‘MinLen (Succ nat0) mono’
                with actual type ‘[t0]’


> minimum [1,2,3]
-- same error as above

> minimum (toMinList (3 :| [2,1]))
1
> minimum (3 `mlcons` toMinLenZero [2,1])
1
```

Here we used Data.List.NonEmpty combined with toMinList or we just work with a List and prove through the usage of cons that it has more than one element.



Adding instances
----------------

If you have a _polymorphic_ data type which is a member of one of the relevant typeclasses ([Functor](http://hackage.haskell.org/package/base/docs/Data-Functor.html),
[Foldable](http://hackage.haskell.org/package/base/docs/Data-Foldable.html),
[Traversable](http://hackage.haskell.org/package/base/docs/Data-Traversable.html)), its quite easy to add an instance for
[MonoFunctor](https://hackage.haskell.org/package/mono-traversable/docs/Data-MonoTraversable.html#t:MonoFunctor), [MonoFoldable](https://hackage.haskell.org/package/mono-traversable/docs/Data-MonoTraversable.html#t:MonoFoldable) or [MonoTraversable](https://hackage.haskell.org/package/mono-traversable/docs/Data-MonoTraversable.html#t:MonoTraversable).

You just have to declare the proper type instance:

``` haskell
{-# LANGUAGE TypeFamilies         #-}

type instance Element (CustomType a) = a
```

And then, we can use the default implementation to declare instances:

``` haskell
instance MonoFunctor (CustomType a)
instance MonoFoldable (CustomType a)
instance MonoTraversable (CustomType a)
```
    
Now you are ready to use ```CustomType a``` with the functions defined in this package.

**Note**: if your type is as _monomorphic_ container without the proper typeclasses, then you will have to provide an implementation rather than using the default. However, this should be fairly simple, as it can be seen [in the code](https://hackage.haskell.org/package/mono-traversable/docs/src/Data-MonoTraversable.html#line-234)


mono-traversable versus lens Traversal
--------------------------------------

lens is a library with a lot of functionality covering a variety pf patterns. One piece of functionality it exposes is `Fold` and `Traversal` which can also be used to deal with monomorphic containers.

You could prefer mono-traversable to using this part of lens because

* Familiar API - If you know `Foldable`, you can use `MonoFoldable` just as easily
* mono-traversable's typeclass based approach means many methods are included in the class but can be given specialised optimized implementations
* You don't explicitly pass around the `Traversal`

The last point is also a point of inflexibility and points to a use case where you could prefer using a lens `Traversal`. mono-traversable treats `ByteString` as a sequence of bytes. If you want to treat it as both bytes and characters, mono-traversable would require a newtype wrapper around `ByteString`, whereas a lens `Traversal` would use a different traversal function.

mono-traversable is only an alternative for `Fold` and `Traversal`, not for `Lens`, `Prism`, `Iso`, `Getter`, `Setter`, `Review`, or `Equality`.




[![Build Status](https://secure.travis-ci.org/snoyberg/mono-traversable.png)](http://travis-ci.org/snoyberg/mono-traversable)
