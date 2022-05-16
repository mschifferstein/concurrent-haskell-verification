# Issue 1: instance declarations with C #
In `C.hs`, the instance declarations are not allowed unless `{-# LANGUAGE TypeSynonymInstances #-}` is added as a flag.

Next, it complains that `Functor (C m)` etc. are not allowed because `C` takes two arguments, while only one is given.

Monad transformer instances in Haskell, e.g. `MaybeT`, are implemented similarly, however they look like this:
```
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
...
instance (Functor m) => Functor (MaybeT m) where
    fmap f = mapMaybeT (fmap (fmap f))
```
I.e. they are defined as `newtype` rather than `type`. So perhaps we could do:
```
newtype C m a = C { getC :: (a -> Action m) -> Action m }
```

# Issue 2: solving instance constraints for CWriter #

