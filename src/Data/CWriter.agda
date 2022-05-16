module Data.CWriter where

open import Haskell.Prelude
open import Data.Writer
open import Data.C
open import Data.MonadTrans

-- instance
--     iWriterC : ⦃ Writer m ⦄ → Writer (C m)
--     iWriterC .write s = lift (write s) -- every write becomes an atomic action

liftedWrite : String → (C W) ⊤
liftedWrite s = lift (write s)
    
-- {-# NON_TERMINATING #-} -- TODO: what are the consequences of this?
-- loop : ⦃ Writer m ⦄ → String → m ⊤
-- loop s = do
--             write s
            -- loop s

loop : String → C W ⊤
loop s = do
            liftedWrite s

example : C W ⊤
-- example = do
--             write "start!"
            -- fork (loop "fish")
            -- loop "cat"
example = liftedWrite "start!" >> loop "cat"

-- example : ⦃ Writer m ⦄ → C m ⊤
-- example = do
--             write "start!"
--             fork (loop "fish")
--             loop "cat"
-- example {{iWriter}} = write {{iWriter}} "start!" >> {!   !} -- this doesn't work

-- example = write "start!" -- this works
-- example = do {! 
--                write "start!"
--                fork (loop "fish")
--                loop "cat"  !}