module Data.Channel where

import Data.MVar

import Data.C

type Stream a = MVar (It a)

data It a = Item a (Stream a)

data Chan a = Channel (MVar (Stream a)) (MVar (Stream a))

newChan :: C IO (Chan a)
newChan
  = newEmptyMVar >>=
      \ hole ->
        newMVar hole >>=
          \ readVar ->
            newMVar hole >>= \ writeVar -> return (Channel readVar writeVar)

writeChan :: Chan a -> a -> C IO ()
writeChan (Channel _ w) val
  = newEmptyMVar >>=
      \ newHole ->
        takeMVar w >>=
          \ oldHole ->
            writeMVar oldHole (Item val newHole) >> writeMVar w newHole

readChan :: Chan a -> C IO a
readChan (Channel r _)
  = takeMVar r >>=
      \ stream ->
        readMVar stream >>=
          \
              (Item val tl) -> writeMVar r tl >> return val

dupChan :: Chan a -> C IO (Chan a)
dupChan (Channel _ w)
  = readMVar w >>=
      \ hole ->
        newMVar hole >>= \ newReadVar -> return (Channel newReadVar w)

ungetChan :: Chan a -> a -> C IO ()
ungetChan (Channel r _) val
  = newEmptyMVar >>=
      \ newReadEnd ->
        takeMVar r >>=
          \ readEnd ->
            writeMVar newReadEnd (Item val readEnd) >> writeMVar r newReadEnd

