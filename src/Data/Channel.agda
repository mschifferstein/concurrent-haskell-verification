module Data.Channel where

open import Haskell.Prelude
open import Data.MVar
open import Data.C

Stream : (a : Set) → Set

{-# NO_POSITIVITY_CHECK #-} -- TODO: check if other definition possible
-- record It (a : Set) : Set where
--     constructor Item
--     field
--         value : a
--         stream : Stream a
data It (a : Set) : Set where
    Item : (x : a) (stream : Stream a) → It a

Stream a = MVar (It a)

data Chan (a : Set) : Set where
    Channel : ( m1 : MVar (Stream a)) (m2 : MVar (Stream a)) → Chan a

newChan : C IO (Chan a)
newChan = do
                hole ← newEmptyMVar
                readVar ← newMVar hole
                writeVar ← newMVar hole
                return (Channel readVar writeVar)

writeChan : Chan a → a → C IO ⊤
writeChan (Channel _ writeVar) val = do
                                        newHole ← newEmptyMVar
                                        oldHole ← readMVar writeVar
                                        writeMVar oldHole (Item val newHole)
                                        writeMVar writeVar newHole

readChan : Chan a → C IO a
readChan (Channel r _) = do
                            stream ← readMVar r
                            Item val tail ← readMVar stream
                            writeMVar r tail
                            return val
