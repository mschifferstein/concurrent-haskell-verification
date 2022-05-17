module Data.CWriter where

open import Haskell.Prelude
open import Data.Writer
open import Data.C
open import Data.MonadTrans
{-# FOREIGN AGDA2HS import Data.Writer #-}
{-# FOREIGN AGDA2HS import Data.C #-}
{-# FOREIGN AGDA2HS import Data.MonadTrans #-}

instance
    iWriterC : ⦃ Writer m ⦄ → Writer (C m)
    iWriterC .write s = lift (write s) -- every write becomes an atomic action
    {-# COMPILE AGDA2HS iWriterC #-}
    
{-# NON_TERMINATING #-} -- TODO: what are the consequences of this?
loop : ⦃ Writer m ⦄ → String → m ⊤
loop s = do
            write s
            loop s
{-# COMPILE AGDA2HS loop #-}

example : ⦃ Writer m ⦄ → C m ⊤
example = do
            write "start!"
            fork (loop "fish")
            loop "cat"
{-# COMPILE AGDA2HS example #-}