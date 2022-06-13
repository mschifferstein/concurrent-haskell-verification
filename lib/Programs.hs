module Programs where

import Data.MonadTrans

import Data.C as C

import Data.IOs

import Data.MVarIOs

-- To convert regular integers to the MyNat data type used by C.run

natToNat :: Int -> C.MyNat

natToNat n | n <= 0 = C.Zero

           | otherwise = C.Suc (natToNat (n-1))

fuel :: MyNat
fuel = natToNat 9223372036854775800

mVarDeadlock :: Bool
mVarDeadlock
  = runIOs
      (run
         (newEmptyMVar >>=
            \ a ->
              newEmptyMVar >>=
                \ b ->
                  fork (takeMVar a fuel >> writeMVar b 1 fuel) >>
                    fork (takeMVar b fuel >> writeMVar a 2 fuel))
         (natToNat 100000))

simpleDeadlock :: Bool
simpleDeadlock
  = runIOs
      (run (newEmptyMVar >>= \ a -> takeMVar a fuel) (natToNat 100000))

simple :: Bool
simple
  = runIOs
      (run (newEmptyMVar >>= \ var -> writeMVar var 5 fuel)
         (natToNat 100000))

