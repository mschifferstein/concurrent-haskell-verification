module Data.Writer where

open import Haskell.Prelude

record W (a : Set) : Set where
    constructor MkW
    field
        value : a
        msg : String
open W public
{-# COMPILE AGDA2HS W #-}

record Writer (m : Set → Set) : Set₁ where
    field
        write : String → m ⊤
        overlap ⦃ super ⦄ : Monad m -- ensures that every Writer is a Monad (only for Haskell classes!)
open Writer ⦃...⦄ public
{-# COMPILE AGDA2HS Writer class #-} -- note the 'class' added here to ensure it translates to a Haskell class.

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

    iWriterW : Writer W
    iWriterW .write s = MkW tt s
    {-# COMPILE AGDA2HS iWriterW #-}

output : W a → String
output x = x .msg
{-# COMPILE AGDA2HS output #-}