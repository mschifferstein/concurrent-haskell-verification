module Data.MVarIOs where

open import Haskell.Prelude
open import Data.C
open import Data.MonadTrans
open import Data.IOs
{-# FOREIGN AGDA2HS import Data.C #-}
{-# FOREIGN AGDA2HS import Data.MonadTrans #-}
{-# FOREIGN AGDA2HS import Data.IOs #-}
-- {-# FOREIGN AGDA2HS import Data.IORef #-} -- built into Haskell

-- Version which uses IOs instead of postulates

MVar : Set
MVar = IORef Data
{-# COMPILE AGDA2HS MVar #-}

newEmptyMVar : C IOs MVar
newEmptyMVar = lift $ newIORef (Nothing , True)
{-# COMPILE AGDA2HS newEmptyMVar #-}

newMVar : Nat → C IOs MVar
newMVar n = lift $ newIORef (Just n , True)
{-# COMPILE AGDA2HS newMVar #-}

checkWriteOk : MVar → IOs Data
checkWriteOk v = do
                v1 ← readIORef v
                writeIORef v (fst v1 , False)
                return v1
{-# COMPILE AGDA2HS checkWriteOk #-}

endWrite : MVar → Nat → IOs ⊤
endWrite v n = writeIORef v (Just n , True)
{-# COMPILE AGDA2HS endWrite #-}

-- This version of writeMVar blocks when the MVar is full - in contrast to Claessen 1999, more true to actual (current) Haskell implementation.
-- According to the semantics of Peyton Jones (1996), writing to a full MVar throws an error instead.
-- Corresponds to putMVar in Control.Concurrent
-- Fuel solves termination issue, at the cost of more convoluted Maybe return type
writeMVar : MVar → Nat → MyNat → C IOs (Maybe ⊤)
writeMVar v n Zero = return Nothing
writeMVar v n (Suc fuel) = do
                            v1 ← lift (checkWriteOk v)
                            case v1 of λ 
                                {(Nothing , True) → (do 
                                                        x <- lift (endWrite v n)
                                                        return (Just x));
                                (Just b , True) →  lift (endWrite v b) >> writeMVar v n fuel;
                                (_ , False) → writeMVar v n fuel}
{-# COMPILE AGDA2HS writeMVar #-}

takeIORef : MVar → IOs (Maybe Nat)
takeIORef v = do
                v1 ← readIORef v
                writeIORef v (Nothing , True)
                return $ fst v1
{-# COMPILE AGDA2HS takeIORef #-}

-- This is Claessen 1999's readMVar
-- Fuel solves termination issue, at the cost of more convoluted Maybe return type
takeMVar : MVar → MyNat → C IOs (Maybe Nat)
takeMVar _ Zero = return Nothing
takeMVar v (Suc n) = do
                        v1 ← lift (takeIORef v)
                        case v1 of λ 
                            {Nothing → takeMVar v n;
                            (Just a) → return (Just a)}
{-# COMPILE AGDA2HS takeMVar #-}


takeIORef2 : MVar → IOs (Maybe Nat)
takeIORef2 v = do
                v1 ← readIORef v -- doesn't remove value from MVar
                return $ fst v1
{-# COMPILE AGDA2HS takeIORef2 #-}

-- This definition allows for multiple reads (value is not removed after being read)
-- Fuel solves termination issue, at the cost of more convoluted Maybe return type
readMVar : MVar → MyNat → C IOs (Maybe Nat)
readMVar _ Zero = return Nothing
readMVar v (Suc n) = do
                        v1 ← lift (takeIORef2 v)
                        case v1 of λ 
                            {Nothing → readMVar v n;
                            (Just a) → return (Just a)}
{-# COMPILE AGDA2HS readMVar #-}