module Data.MVar where

open import Haskell.Prelude
open import Data.C
open import Data.MonadTrans
{-# FOREIGN AGDA2HS import Data.C #-}
{-# FOREIGN AGDA2HS import Data.MonadTrans #-}
{-# FOREIGN AGDA2HS import Data.IORef #-} -- built into Haskell

-- TODO: resolve non-termination by introducing time-outs?

postulate 
    IO : Set → Set
    IORef : Set → Set -- corresponds to Haskell's IORef
    newIORef : a → IO (IORef a)
    readIORef : IORef a → IO a
    writeIORef : IORef a → a → IO ⊤

instance 
    postulate
        iMonadIO : Monad IO

MVar : (a : Set) → Set
MVar a = IORef (Maybe a × Bool)
{-# COMPILE AGDA2HS MVar #-}

newEmptyMVar : { @0 a : Set } → C IO (MVar a)
newEmptyMVar = lift (do
                        v ← newIORef (Nothing , True)
                        return v)
{-# COMPILE AGDA2HS newEmptyMVar #-}

newMVar : a → C IO (MVar a)
newMVar a = lift (newIORef ((Just a , True)))
{-# COMPILE AGDA2HS newMVar #-}

checkWriteOk : MVar a → IO (Maybe a × Bool)
checkWriteOk v = do
                v1 ← readIORef v
                writeIORef v (fst v1 , False)
                return v1
{-# COMPILE AGDA2HS checkWriteOk #-}

endWrite : MVar a → a → IO ⊤
endWrite v a = writeIORef v (Just a , True)
{-# COMPILE AGDA2HS endWrite #-}

-- This version of writeMVar blocks when the MVar is full - in contrast to Claessen 1999, more true to actual (current) Haskell implementation.
-- According to the semantics of Peyton Jones (1996), writing to a full MVar throws an error instead.
{-# NON_TERMINATING #-}
writeMVar : MVar a → a → C IO ⊤
writeMVar v a = do
                    v1 ← lift (checkWriteOk v)
                    case v1 of λ 
                        {(Nothing , True) → lift (endWrite v a);
                         (Just b , True) →  lift (endWrite v b) >> writeMVar v a;
                         (_ , False) → writeMVar v a}
{-# COMPILE AGDA2HS writeMVar #-}

takeIORef : MVar a → IO (Maybe a)
takeIORef v = do
                v1 ← readIORef v
                writeIORef v (Nothing , True)
                return $ fst v1
{-# COMPILE AGDA2HS takeIORef #-}

-- This is Claessen 1999's readMVar
{-# NON_TERMINATING #-}
takeMVar : MVar a → C IO a
takeMVar v = do
                v1 ← lift (takeIORef v)
                case v1 of λ 
                    {Nothing → takeMVar v;
                     (Just a) → return a}
{-# COMPILE AGDA2HS takeMVar #-}

-- This definition allows for multiple reads (value is not removed after being read)
{-# NON_TERMINATING #-}
readMVar : MVar a → C IO a
readMVar v = do
                v1 ← lift (readIORef v)
                case fst v1 of λ 
                    {Nothing → readMVar v;
                     (Just a) → return a}
{-# COMPILE AGDA2HS readMVar #-}