module Data.Channel where

open import Haskell.Prelude
open import Data.MVar
open import Data.C
{-# FOREIGN AGDA2HS import Data.MVar #-}
{-# FOREIGN AGDA2HS import Data.C #-}

Stream : (a : Set) → Set

{-# NO_POSITIVITY_CHECK #-} -- TODO: check if other definition possible
-- record It (a : Set) : Set where
--     constructor Item
--     field
--         value : a
--         stream : Stream a
data It (a : Set) : Set where
    Item : (x : a) (stream : Stream a) → It a
{-# COMPILE AGDA2HS It #-}

Stream a = MVar (It a)
{-# COMPILE AGDA2HS Stream #-}

data Chan (a : Set) : Set where
    Channel : MVar (Stream a) → MVar (Stream a) → Chan a
{-# COMPILE AGDA2HS Chan #-}

newChan : C IO (Chan a)
newChan = do
                hole ← newEmptyMVar
                readVar ← newMVar hole
                writeVar ← newMVar hole
                return (Channel readVar writeVar)
{-# COMPILE AGDA2HS newChan #-}

writeChan : Chan a → a → C IO ⊤
writeChan (Channel _ w) val = do
                                        newHole ← newEmptyMVar
                                        oldHole ← takeMVar w
                                        writeMVar oldHole (Item val newHole)
                                        writeMVar w newHole
{-# COMPILE AGDA2HS writeChan #-}

readChan : Chan a → C IO a
readChan (Channel r _) =
                        do
                            stream ← takeMVar r
                            (Item val tl) ← readMVar stream -- requires LambdaCase among the default-extensions in project.cabal
                            writeMVar r tl
                            return val
                            
                        -- Why does this desugared syntax not work??
                        -- takeMVar r >>= λ s → 
                        --                 readMVar s >>= λ (Item val tl) → 
                        --                                 writeMVar r tl >> return val
{-# COMPILE AGDA2HS readChan #-}

dupChan : Chan a → C IO (Chan a)
dupChan (Channel _ w) = do
                            hole ← readMVar w
                            newReadVar ← newMVar hole
                            return (Channel newReadVar w)
{-# COMPILE AGDA2HS dupChan #-}

-- This implementation is faulty!
-- A deadlock can occur when the channel is empty and a thread is already waiting to read (it's blocked on the readVar)
ungetChan : Chan a → a → C IO ⊤
ungetChan (Channel r _) val = do
                                newReadEnd ← newEmptyMVar
                                readEnd ← takeMVar r
                                writeMVar newReadEnd (Item val readEnd)
                                writeMVar r newReadEnd
{-# COMPILE AGDA2HS ungetChan #-} 