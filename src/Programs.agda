open import Haskell.Prelude
open import Data.C as C
open import Data.MVarIOs as MVar
open import Data.IOs as IOs
open import Data.Channel as Channel
open import Data.ProofLib
open import Data.MonadTrans as MonadTrans
open import Agda.Builtin.Nat
{-# FOREIGN AGDA2HS import Data.MonadTrans #-}
{-# FOREIGN AGDA2HS import Data.C as C#-}
{-# FOREIGN AGDA2HS import Data.IOs #-}
{-# FOREIGN AGDA2HS import Data.MVarIOs #-}

natToNat : Nat -> MyNat
natToNat zero = C.Zero
natToNat (suc n) = C.Suc (natToNat n)

{-# FOREIGN AGDA2HS 
-- To convert regular integers to the MyNat data type used by C.run
natToNat :: Int -> C.MyNat
natToNat n | n <= 0 = C.Zero
           | otherwise = C.Suc (natToNat (n-1))
 #-}

fuel = natToNat 9223372036854775800
{-# COMPILE AGDA2HS fuel #-}

mVarDeadlock : Bool
mVarDeadlock = runIOs (C.run (do
                a <- MVar.newEmptyMVar
                b <- MVar.newEmptyMVar
                C.fork (do
                            MVar.takeMVar a fuel
                            MVar.writeMVar b 1 fuel)
                C.fork (do
                            MVar.takeMVar b fuel
                            MVar.writeMVar a 2 fuel)
                ) (natToNat 100000))
{-# COMPILE AGDA2HS mVarDeadlock #-}

deadlock-proof : mVarDeadlock ≡ False
deadlock-proof = refl

simpleDeadlock : Bool
simpleDeadlock = runIOs (C.run (do
                a <- MVar.newEmptyMVar
                MVar.takeMVar a fuel
                ) (natToNat 100000))
{-# COMPILE AGDA2HS simpleDeadlock #-}

simple-deadlock-proof : simpleDeadlock ≡ False
simple-deadlock-proof = refl

-- ungetChanDeadlock : IO ⊤
-- ungetChanDeadlock = C.run (do
--                             chan <- Channel.newChan
--                             C.fork (do
--                                         val <- Channel.readChan chan
--                                         return val)
--                             Channel.ungetChan chan false
--                             ) (natToNat 9223372036854775800)

-- incrementValTwice : Bool
-- -- incrementValTwice : MVar Nat → C IO Nat
-- incrementValTwice = run-s (C.run (do
--                             var <- MVar.newMVar 0
--                             C.par (do
--                                         val <- MVar.readMVar var fuel -- works with takeMVar, but not with readMVar...
--                                         MVar.writeMVar var (val Agda.Builtin.Nat.+ 1) fuel)
--                                   (do
--                                         val <- MVar.readMVar var fuel -- TODO: extract value from Maybe
--                                         MVar.writeMVar var (val Agda.Builtin.Nat.+ 1) fuel)
--                             -- MVar.takeMVar var
--                         ) 
--                         -- (natToNat 9223372036854775800)
--                         (natToNat 100))

simple : Bool
simple = runIOs (C.run (do
                  var <- MVar.newEmptyMVar
                  MVar.writeMVar var 5 fuel)
            (natToNat 100000))
{-# COMPILE AGDA2HS simple #-}

inc-terminates : simple ≡ True
inc-terminates = refl