module Programs where

open import Haskell.Prelude
open import Data.C
open import Data.MVarIOs
open import Data.IOs
open import Data.MonadTrans
open import Agda.Builtin.Nat
{-# FOREIGN AGDA2HS import Data.MonadTrans #-}
{-# FOREIGN AGDA2HS import Data.C #-}
{-# FOREIGN AGDA2HS import Data.IOs #-}
{-# FOREIGN AGDA2HS import Data.MVarIOs #-}

natToMyNat : Nat -> MyNat
natToMyNat zero = Zero
natToMyNat (suc n) = Suc (natToMyNat n)

{-# FOREIGN AGDA2HS 
-- To convert regular integers to the MyNat data type used by C.run
natToMyNat :: Int -> MyNat
natToMyNat n | n <= 0 = Zero
           | otherwise = Suc (natToMyNat (n-1))
 #-}

fuel = natToMyNat 9223372036854775800
{-# COMPILE AGDA2HS fuel #-}

-- A simple program that deadlocks because both threads are waiting to read from an empty MVar that the other thread needs to write to.
mVarDeadlock : Bool
mVarDeadlock = runIOs (run (do
                a <- newEmptyMVar
                b <- newEmptyMVar
                fork (do
                        takeMVar a fuel
                        writeMVar b 1 fuel)
                takeMVar b fuel
                writeMVar a 2 fuel
                ) (natToMyNat 100000))
{-# COMPILE AGDA2HS mVarDeadlock #-}

deadlock-proof : mVarDeadlock ≡ False
deadlock-proof = refl

-- Alternative version of the program that does not deadlock.
mVarNoDeadlock : Bool
mVarNoDeadlock = runIOs (run (do
                a <- newEmptyMVar
                b <- newEmptyMVar
                fork (do
                        takeMVar a fuel
                        writeMVar b 1 fuel)
                writeMVar a 2 fuel
                takeMVar b fuel
                ) (natToMyNat 100000))
{-# COMPILE AGDA2HS mVarNoDeadlock #-}

no-deadlock-proof : mVarNoDeadlock ≡ True
no-deadlock-proof = refl

-- Trivial deadlock example
simpleDeadlock : Bool
simpleDeadlock = runIOs (run (do
                a <- newEmptyMVar
                takeMVar a fuel
                ) (natToMyNat 100000))
{-# COMPILE AGDA2HS simpleDeadlock #-}

simple-deadlock-proof : simpleDeadlock ≡ False
simple-deadlock-proof = refl

-- Trivial terminating program
simple : Bool
simple = runIOs (run (do
                  var <- newEmptyMVar
                  writeMVar var 5 fuel)
            (natToMyNat 100000))
{-# COMPILE AGDA2HS simple #-}

simple-terminates : simple ≡ True
simple-terminates = refl

-- Program that deadlocks in a genuine concurrent setting, but terminates in our deterministic round-robin model.
failDetect : Bool
failDetect = runIOs (run (do
                a <- newEmptyMVar
                mutex <- newMVar 0
                fork (do
                        takeMVar mutex fuel 
                        writeMVar a 2 fuel
                        writeMVar mutex 0 fuel)
                takeMVar mutex fuel
                takeMVar a fuel
                writeMVar mutex 0 fuel
                ) (natToMyNat 100000))
{-# COMPILE AGDA2HS failDetect #-}

fail-proof : failDetect ≡ True
fail-proof = refl

-- Program in which it is unclear what the final value written to newVar will be.
-- Would be interesting in combination with a proof about memory contents.
whatsfinal : Bool
whatsfinal = runIOs (run (do 
                            var <- newMVar 0
                            fork (do
                                        val <- takeMVar var fuel
                                        case val of λ
                                            {Nothing → return Nothing;
                                             (Just x) → writeMVar var (x Agda.Builtin.Nat.+ 1) fuel})
                            fork (do
                                        val <- takeMVar var fuel
                                        case val of λ
                                            {Nothing → return Nothing;
                                             (Just x) → writeMVar var (x Agda.Builtin.Nat.+ 1) fuel})
                            val <- readMVar var fuel
                            newVar <- newEmptyMVar
                            case val of λ
                                            {Nothing → return Nothing;
                                             (Just x) → writeMVar newVar x fuel}
                        )
                        (natToMyNat 100000))

whatsfinal-terminates : whatsfinal ≡ True
whatsfinal-terminates = refl