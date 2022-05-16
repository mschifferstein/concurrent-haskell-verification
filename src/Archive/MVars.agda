module Data.MVars where

open import Agda.Builtin.Unit
open import Haskell.Prelude

record MVars : Set₁ where
    field
        MVar : Set → Set
        IO : Set → Set
        newEmptyMVar : {a : Set } → IO (MVar a)
        newMVar : (a : Set) → IO (MVar a)
        takeMVar : { a : Set} → MVar a → IO a
        putMVar : { a : Set} → MVar a -> a -> IO ⊤
open MVars ⦃...⦄ public

{-# COMPILE AGDA2HS MVars #-} 