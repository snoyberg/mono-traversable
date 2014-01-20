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
    

in your code, and your ready to use ```CustomType a``` with the functions defined in this package.

**Note**: if your type is as _monomorphic container_ without the proper typeclasses, then you will have to provide an implementation. However, this should be fairly simple, as it can be seen [in the code](https://hackage.haskell.org/package/mono-traversable-0.2.0.0/docs/src/Data-MonoTraversable.html#line-234)


[![Build Status](https://secure.travis-ci.org/snoyberg/mono-traversable.png)](http://travis-ci.org/snoyberg/mono-traversable)
