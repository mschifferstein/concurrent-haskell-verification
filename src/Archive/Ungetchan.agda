module Archive.Ungetchan where

open import Haskell.Prelude
open import Data.Channel
open import Data.MVar
open import Data.C

natToMyNat : Nat -> MyNat
natToMyNat zero = Zero
natToMyNat (suc n) = Suc (natToMyNat n)

{-
This program shows that the ungetChan method can lead to a deadlock.
Not made to work with IOs model, so no proofs can be written about it, because MVarIOs only supports natural numbers as content at present.
-}
ungetChanDeadlock : IO Bool
ungetChanDeadlock = run (do
                            chan <- newChan
                            fork (do
                                        val <- readChan chan
                                        return val)
                            ungetChan chan false
                            ) (natToMyNat 9223372036854775800)