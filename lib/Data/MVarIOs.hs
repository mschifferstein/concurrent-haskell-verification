module Data.MVarIOs where

import Numeric.Natural (Natural)

import Data.C

import Data.MonadTrans

import Data.IOs

type MVar = IORef Data

newEmptyMVar :: C IOs MVar
newEmptyMVar = lift $ newIORef (Nothing, True)

newMVar :: Natural -> C IOs MVar
newMVar n = lift $ newIORef (Just n, True)

checkWriteOk :: MVar -> IOs Data
checkWriteOk v
  = readIORef v >>= \ v1 -> writeIORef v (fst v1, False) >> return v1

endWrite :: MVar -> Natural -> IOs ()
endWrite v n = writeIORef v (Just n, True)

writeMVar :: MVar -> Natural -> MyNat -> C IOs (Maybe ())
writeMVar v n Zero = return Nothing
writeMVar v n (Suc fuel)
  = lift (checkWriteOk v) >>=
      \ v1 ->
        case v1 of
            (Nothing, True) -> lift (endWrite v n) >>= \ x -> return (Just x)
            (Just b, True) -> lift (endWrite v b) >> writeMVar v n fuel
            (_, False) -> writeMVar v n fuel

takeIORef :: MVar -> IOs (Maybe Natural)
takeIORef v
  = readIORef v >>=
      \ v1 -> writeIORef v (Nothing, True) >> (return $ fst v1)

takeMVar :: MVar -> MyNat -> C IOs (Maybe Natural)
takeMVar _ Zero = return Nothing
takeMVar v (Suc n)
  = lift (takeIORef v) >>=
      \ v1 ->
        case v1 of
            Nothing -> takeMVar v n
            Just a -> return (Just a)

takeIORef2 :: MVar -> IOs (Maybe Natural)
takeIORef2 v = readIORef v >>= \ v1 -> return $ fst v1

readMVar :: MVar -> MyNat -> C IOs (Maybe Natural)
readMVar _ Zero = return Nothing
readMVar v (Suc n)
  = lift (takeIORef2 v) >>=
      \ v1 ->
        case v1 of
            Nothing -> readMVar v n
            Just a -> return (Just a)

