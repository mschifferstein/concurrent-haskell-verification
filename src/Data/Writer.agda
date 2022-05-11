module Data.Writer where

open import Haskell.Prelude
import Haskell.Prim.Monad as Monad
import Haskell.Prim.Applicative as Applicative
import Haskell.Prim.Functor as Functor
import Haskell.Prim.String as String
import Haskell.Prim.Tuple as Tuple

W : Set → Set
W a = a × String
{-# COMPILE AGDA2HS W #-}

-- data W (a : Set) : Set where
--     w : a → a × String
-- {-# COMPILE AGDA2HS W #-}

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