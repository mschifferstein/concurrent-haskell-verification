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

