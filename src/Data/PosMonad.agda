module Data.PosMonad where

open import Haskell.Prelude
open import Data.Writer

data PosMonad : Set where
    posWriter : PosMonad
{-# COMPILE AGDA2HS PosMonad #-} 

toMonad : PosMonad → (Set → Set)
toMonad posWriter = W
-- toMonad posWriter = Maybe
{-# COMPILE AGDA2HS toMonad #-}