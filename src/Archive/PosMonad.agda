module Archive.PosMonad where

open import Haskell.Prelude
open import Data.Swierstra

-- W as a tuple is not strictly positive.
-- Also couldn't yet get it to work with only IOs as positive monad.

W : Set → Set
W a = a × String

record Writer (m : Set → Set) : Set₁ where
    field
        write : String → m ⊤
        overlap ⦃ super ⦄ : Monad m -- ensures that every Writer is a Monad (only for Haskell classes!)
open Writer ⦃...⦄ public
{-# COMPILE AGDA2HS Writer class #-} -- note the 'class' added here to ensure it translates to a Haskell class.

instance
    iFunctorW : Functor W
    iFunctorW .fmap f (x , s)   = (f x) , s
    {-# COMPILE AGDA2HS iFunctorW #-}

    iApplicativeW : Applicative W
    iApplicativeW .pure a                        = a , ""
    iApplicativeW ._<*>_ (f , s) (x , s')    = (f x) , (s ++ s')
    {-# COMPILE AGDA2HS iApplicativeW #-}

    iMonadW : Monad W
    iMonadW ._>>=_ (x , s) k   = (fst (k x)) , (s ++ snd (k x))
    {-# COMPILE AGDA2HS iMonadW #-}

    iWriterW : Writer W
    iWriterW .write s = tt , s
    {-# COMPILE AGDA2HS iWriterW #-}

output : W a → String
output x = snd x
{-# COMPILE AGDA2HS output #-}

data PosMonad : Set where
    -- posWriter : PosMonad
    posIOs : PosMonad
{-# COMPILE AGDA2HS PosMonad #-} 

toMonad : PosMonad → (Set → Set)
-- toMonad posWriter = W
toMonad posIOs = IOs
{-# COMPILE AGDA2HS toMonad #-}

-- Trying to prove here that every positive monad is a functor.
iFunctorPMProof : (m : PosMonad) -> Functor (toMonad m)
iFunctorPMProof posIOs = iFunctorIOs

-- Trying to prove here that every positive monad is a monad.
iMonadPMProof : (m : PosMonad) -> Monad (toMonad m)
iMonadPMProof posIOs = iMonadIOs
{-# COMPILE AGDA2HS iMonadPMProof #-}