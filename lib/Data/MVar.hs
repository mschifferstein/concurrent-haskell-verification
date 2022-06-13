module Data.MVar where

import Data.C

import Data.MonadTrans

import Data.IORef

type MVar a = IORef (Maybe a, Bool)

newEmptyMVar :: C IO (MVar a)
newEmptyMVar = lift (newIORef (Nothing, True))

newMVar :: a -> C IO (MVar a)
newMVar a = lift (newIORef (Just a, True))

checkWriteOk :: MVar a -> IO (Maybe a, Bool)
checkWriteOk v = readIORef v >>= \ v1 -> Id () >> return v1

endWrite :: MVar a -> a -> IO ()
endWrite v a = writeIORef v (Just a, True)

writeMVar :: MVar a -> a -> C IO ()
writeMVar v a
  = lift (checkWriteOk v) >>=
      \ v1 ->
        case v1 of
            (Nothing, True) -> lift (Id ())
            (Just b, True) -> lift (Id ()) >> writeMVar v a
            (_, False) -> writeMVar v a

takeIORef :: MVar a -> IO (Maybe a)
takeIORef v = readIORef v >>= \ v1 -> Id () >> (return $ fst v1)

takeMVar :: MVar a -> C IO a
takeMVar v
  = lift (takeIORef v) >>=
      \ v1 ->
        case v1 of
            Nothing -> takeMVar v
            Just a -> return a

takeIORef2 :: MVar a -> IO (Maybe a)
takeIORef2 v = readIORef v >>= \ v1 -> Id () >> (return $ fst v1)

readMVar :: MVar a -> C IO a
readMVar v
  = lift (takeIORef2 v) >>=
      \ v1 ->
        case v1 of
            Nothing -> readMVar v
            Just a -> return a

