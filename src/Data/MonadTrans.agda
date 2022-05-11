module Data.MonadTrans where

open import Haskell.Prelude

record MonadTrans (τ : (Set → Set) → (Set → Set)) : Set₁ where
    field
        lift : ⦃ Monad m ⦄ → m a → (τ m) a
open MonadTrans ⦃...⦄ public
{-# COMPILE AGDA2HS MonadTrans class #-}