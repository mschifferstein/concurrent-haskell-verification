module Data.PosMonad where

open import Haskell.Prelude
open import Data.Writer
-- open import Data.WriterTuple

data PosMonad : Set where
    posWriter : PosMonad
{-# COMPILE AGDA2HS PosMonad #-} 

toMonad : PosMonad → (Set → Set)
toMonad posWriter = W
{-# COMPILE AGDA2HS toMonad #-}

-- iFunctorPMProof : (m : PosMonad) -> Functor (toMonad m)
-- iFunctorPMProof posWriter = iFunctorW
-- iFunctorPMProof : { m : PosMonad } -> Functor (toMonad m)
-- iFunctorPMProof = {! iFunctorW  !}

-- iMonadPMProof : (m : PosMonad) -> Monad (toMonad m)
-- iMonadPMProof posWriter = iMonadW
-- {-# COMPILE AGDA2HS iMonadPMProof #-}