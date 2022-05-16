module Data.MonadTrans where

open import Haskell.Prelude

record MonadTrans (t : (Set → Set) → (Set → Set)) : Set₁ where
    field
        lift : ⦃ Monad m ⦄ → m a → (t m) a
open MonadTrans ⦃...⦄ public
{-# COMPILE AGDA2HS MonadTrans class #-}