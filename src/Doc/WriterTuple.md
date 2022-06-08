With how Tuples are defined in Agda2hs, Action would not be strictly positive when posWriter is used as equivalent to W.
Therefore, W is represented as a record with fields instead.

Previously, W was defined as:
```
W : Set → Set
W a = a × String
{-# COMPILE AGDA2HS W #-}

record Writer (m : Set → Set) : Set₁ where
    field
        write : String → m ⊤
        overlap ⦃ super ⦄ : Monad m -- ensures that every Writer is a Monad (only for Haskell classes!)
open Writer ⦃...⦄ public
{-# COMPILE AGDA2HS Writer class #-} -- note the 'class' added here to ensure it translates to a Haskell class.

instance
    iFunctor : Functor W
    iFunctor .fmap f (a , s) = (f a , s)
    {-# COMPILE AGDA2HS iFunctor #-}

    iApplicative : Applicative W
    iApplicative .pure a = a , []
    iApplicative ._<*>_ (f , s) (a , s') = (f a , s ++ s')
    {-# COMPILE AGDA2HS iApplicative #-}

    iMonad : Monad W
    iMonad ._>>=_ (a , s) k = let 
                            b = fst (k a)
                            s' = snd (k a)
                        in (b , (s ++ s'))
    {-# COMPILE AGDA2HS iMonad #-}

    iWriter : Writer W
    iWriter .write s = (tt , s)
    {-# COMPILE AGDA2HS iWriter #-}


output : W a → String
output (a , s) = s
{-# COMPILE AGDA2HS output #-}
```

Now, we cannot have
```
toMonad : PosMonad → (Set → Set)
toMonad posWriter = W
```
Which is equal to
```
toMonad : PosMonad → (Set → Set)
toMonad posWriter = _× String
```
And we have
```
data Action (m : PosMonad) : Set where
    Atom : (toMonad m) (Action m) → Action m
```
So that means the second argument of toMonad is `(Action m)` which will thus be inserted in the Tuple, while the Tuple is defined as:
```
data Tuple : List Set → Set where
  []  : Tuple []
  _∷_ : a → Tuple as → Tuple (a ∷ as)
```
So that `Action m` appears on the left side of an arrow.