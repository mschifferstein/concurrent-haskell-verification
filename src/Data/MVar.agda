module Data.MVar where

open import Haskell.Prelude
open import Data.C
open import Data.MonadTrans
open import Data.Writer
{-# FOREIGN AGDA2HS import Data.C #-}
{-# FOREIGN AGDA2HS import Data.MonadTrans #-}
{-# FOREIGN AGDA2HS import Data.IORef #-} -- built into Haskell

{- 
Defining the IO monad as the Identity monad rather than just postulating it allows us to extract values from it.
This MVar definition, when translated to Haskell, works with Haskell's built-in IO and IORef.
Note that some functions are still marked as non-terminating and thus cannot be used in proofs yet.
-}

record Identity (a : Set) : Set where
  constructor Id
  field
    runId : a
open Identity public

instance
    FunctorId : Functor Identity
    ApplicativeId : Applicative Identity
    MonadId : Monad Identity

    FunctorId .fmap f x = x >>= (pure ∘ f)

    ApplicativeId .pure = Id
    ApplicativeId ._<*>_ mf ma = do 
                                    f ← mf
                                    a ← ma
                                    pure (f a)

    MonadId ._>>=_ = λ (Id x) f → f x

IO = Identity

postulate 
    -- IO : Set → Set
    IORef : Set → Set -- corresponds to Haskell's IORef
    newIORef : a → IO (IORef a)
    readIORef : IORef a → IO a
    writeIORef : IORef a → a → IO ⊤

-- instance 
--     postulate
--         iMonadIO : Monad IO

MVar : (a : Set) → Set
MVar a = IORef (Maybe a × Bool)
{-# COMPILE AGDA2HS MVar #-}

newEmptyMVar : { @0 a : Set } → C IO (MVar a)
newEmptyMVar = lift (newIORef (Nothing , True))
{-# COMPILE AGDA2HS newEmptyMVar #-}

newMVar : a → C IO (MVar a)
newMVar a = lift (newIORef (Just a , True))
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

{-
This version of writeMVar blocks when the MVar is full - in contrast to Claessen 1999, more true to actual (current) Haskell implementation.
According to the semantics of Peyton Jones (1996), writing to a full MVar throws an error instead.
Corresponds to putMVar in Control.Concurrent
-}
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


takeIORef2 : MVar a → IO (Maybe a)
takeIORef2 v = do
                v1 ← readIORef v -- doesn't remove value from MVar
                return $ fst v1
{-# COMPILE AGDA2HS takeIORef2 #-}

-- This definition allows for multiple reads (value is not removed after being read)
{-# NON_TERMINATING #-}
readMVar : MVar a → C IO a
readMVar v = do
                v1 ← lift (takeIORef2 v)
                case v1 of λ 
                    {Nothing → readMVar v;
                     (Just a) → return a}
{-# COMPILE AGDA2HS readMVar #-} 