module Data.MIORef2 where

open import Haskell.Prelude
open import Data.C
open import Data.MonadTrans
{-# FOREIGN AGDA2HS import Data.C #-}
{-# FOREIGN AGDA2HS import Data.MonadTrans #-}
{-# FOREIGN AGDA2HS import Data.IORef #-} -- built into Haskell

-- This is the version of MVar as given by Claessen 1999.
-- It does not include blocking for writing MVars.

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
MVar a = IORef (Maybe a)
{-# COMPILE AGDA2HS MVar #-}

newEmptyMVar : { @0 a : Set } → C IO (MVar a)
newEmptyMVar = lift (do
                        v ← newIORef Nothing
                        return v)
{-# COMPILE AGDA2HS newEmptyMVar #-}

newMVar : a → C IO (MVar a)
newMVar a = lift (newIORef (Just a))
{-# COMPILE AGDA2HS newMVar #-}

writeMVar : MVar a → a → C IO ⊤
-- This definition is too simple (Claessen 1999), because it doesn't block when the MVar is full
-- According to the semantics of Peyton Jones (1996), writing to a full MVar throws an error
writeMVar v a = lift (writeIORef v (Just a))
{-# COMPILE AGDA2HS writeMVar #-}

takeIORef : MVar a → IO (Maybe a)
takeIORef v = do
                v1 ← readIORef v
                writeIORef v Nothing
                return v1
{-# COMPILE AGDA2HS takeIORef #-}

{-# NON_TERMINATING #-}
readMVar : MVar a → C IO a
readMVar v = do
                v1 ← lift (takeIORef v)
                case v1 of λ 
                    {Nothing → readMVar v;
                     (Just a) → return a}
{-# COMPILE AGDA2HS readMVar #-}