import qualified Programs

-- -- To convert regular integers to the MyNat data type used by C.run
-- intToNat :: Int -> C.MyNat
-- intToNat n | n <= 0 = C.Zero
--            | otherwise = C.Suc (intToNat (n-1))

main :: IO ()
-- main = print $ take 100 (Writer.output (C.run CWriter.example (intToNat 100)))
-- main = ungetChanDeadlock
-- main = mVarDeadlock
main = print $ "This program terminates: " ++ show Programs.mVarDeadlock

-- -- Using MVars in the wrong order can result in a deadlock.
-- -- In this case, two threads are trying to retrieve an empty MVar, and thus waiting for each other to write to it.
-- -- Obvious example.
-- mVarDeadlock :: IO Bool
-- mVarDeadlock = C.run (do
--                 a <- MVar.newEmptyMVar
--                 b <- MVar.newEmptyMVar
--                 C.fork (do
--                             MVar.takeMVar a
--                             MVar.writeMVar b ())
--                 C.fork (do
--                             MVar.takeMVar b
--                             MVar.writeMVar a ())
--                 ) (intToNat 9223372036854775800)

-- -- The ungetChan function can result in a deadlock, when another thread is already waiting to read from an empty channel.
-- ungetChanDeadlock :: IO Bool
-- ungetChanDeadlock = C.run (do
--                             chan <- Channel.newChan
--                             C.fork (do
--                                         val <- Channel.readChan chan
--                                         return val)
--                             Channel.ungetChan chan 5
--                             ) (intToNat 9223372036854775800)

-- incrementValTwice :: IO Bool
-- incrementValTwice = do
--                         C.run (do
--                             var <- MVar.newMVar 0
--                             -- C.fork (do
--                             --             val <- MVar.readMVar var -- works with takeMVar, but not with readMVar...
--                             --             MVar.writeMVar var (val+1)
--                             --             return $ print val)
--                             -- C.fork (do
--                             --             val <- MVar.readMVar var
--                             --             MVar.writeMVar var (val+1))
--                             C.par (do
--                                         val <- MVar.takeMVar var -- works with takeMVar, but not with readMVar...
--                                         MVar.writeMVar var (val+1))
--                                         -- return $ print val)
--                                    (do
--                                         val <- MVar.takeMVar var
--                                         MVar.writeMVar var (val+1))
--                             -- val <- MVar.readMVar var
--                             -- MVar.writeMVar var (val+1)
--                             -- val <- MVar.takeMVar var
--                             -- return val
--                             ) (intToNat 9223372036854775800)