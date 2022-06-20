module Programs where

import Data.MonadTrans

import Data.C

import Data.IOs

import Data.MVarIOs

-- To convert regular integers to the MyNat data type used by C.run

natToMyNat :: Int -> MyNat

natToMyNat n | n <= 0 = Zero

           | otherwise = Suc (natToMyNat (n-1))

fuel :: MyNat
fuel = natToMyNat 9223372036854775800

mVarDeadlock :: Bool
mVarDeadlock
  = runIOs
      (run
         (newEmptyMVar >>=
            \ a ->
              newEmptyMVar >>=
                \ b ->
                  fork (takeMVar a fuel >> writeMVar b 1 fuel) >>
                    (takeMVar b fuel >> writeMVar a 2 fuel))
         (natToMyNat 100000))

mVarNoDeadlock :: Bool
mVarNoDeadlock
  = runIOs
      (run
         (newEmptyMVar >>=
            \ a ->
              newEmptyMVar >>=
                \ b ->
                  fork (takeMVar a fuel >> writeMVar b 1 fuel) >>
                    (writeMVar a 2 fuel >> takeMVar b fuel))
         (natToMyNat 100000))

simpleDeadlock :: Bool
simpleDeadlock
  = runIOs
      (run (newEmptyMVar >>= \ a -> takeMVar a fuel) (natToMyNat 100000))

simple :: Bool
simple
  = runIOs
      (run (newEmptyMVar >>= \ var -> writeMVar var 5 fuel)
         (natToMyNat 100000))

failDetect :: Bool
failDetect
  = runIOs
      (run
         (newEmptyMVar >>=
            \ a ->
              newMVar 0 >>=
                \ mutex ->
                  fork
                    (takeMVar mutex fuel >>
                       (writeMVar a 2 fuel >> writeMVar mutex 0 fuel))
                    >>
                    (takeMVar mutex fuel >>
                       (takeMVar a fuel >> writeMVar mutex 0 fuel)))
         (natToMyNat 100000))

