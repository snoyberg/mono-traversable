mono-traversable
================

Type classes for mapping, folding, and traversing monomorphic containers. Contains even more experimental code for abstracting containers and sequences. 

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
    

As a very simple example, consider ```Either a```. Already a member of ```MonoFunctor```, it was not included as an instance of either ```MonoFoldable``` or ```MonoTraversable```, since it is not defined as either ```Foldable``` or ```Traversable``` in GHC _7.6_.

However, these instances are [fairly](https://ghc.haskell.org/trac/ghc/browser/base/Data/Foldable.hs#L172) [obvious](https://ghc.haskell.org/trac/ghc/browser/base/Data/Traversable.hs#L182), and as such will be entering [GHC 7.8](https://ghc.haskell.org/trac/ghc/wiki/Building/GettingTheSources). Having them, it is then as simple as adding:

```instance MonoFoldable (Either a b)```

and

```instance MonoTraversable (Either a b)```

in your code, and your ready to use ```Either a``` with the functions defined in this package.

**Note**: if your type is as _monomorphic container_ without the proper typeclasses, then you will have to provide an implementation. However, this should be fairly simple, as it can be seen [in the code](https://hackage.haskell.org/package/mono-traversable-0.2.0.0/docs/src/Data-MonoTraversable.html#line-234)


[![Build Status](https://secure.travis-ci.org/snoyberg/mono-traversable.png)](http://travis-ci.org/snoyberg/mono-traversable)
