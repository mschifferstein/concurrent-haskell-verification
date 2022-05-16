module Data.Writer2 where

open import Haskell.Prelude

record W (a : Set) : Set where
    constructor MkW
    field
        value : a
        msg : String
open W public
{-# COMPILE AGDA2HS W #-}

instance
    iFunctorW : Functor W
    iFunctorW .fmap f (MkW x s)   = MkW (f x) s
    {-# COMPILE AGDA2HS iFunctorW #-}

    iApplicativeW : Applicative W
    iApplicativeW .pure a                        = MkW a ""
    iApplicativeW ._<*>_ (MkW f s) (MkW x s')    = MkW (f x) (s ++ s')
    {-# COMPILE AGDA2HS iApplicativeW #-}

    iMonadW : Monad W
    iMonadW ._>>=_ (MkW x s) k   = MkW (k x .value) (s ++ k x .msg)
    {-# COMPILE AGDA2HS iMonadW #-}

write : String → W ⊤
write s = MkW tt s

output : W a → String
output x = x .msg
{-# COMPILE AGDA2HS output #-}