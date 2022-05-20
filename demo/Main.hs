import qualified Data.C as C
import qualified Data.Writer as Writer
import qualified Data.CWriter as CWriter
import qualified Data.MVar as MVar
import qualified Data.Channel as Channel

-- To convert regular integers to the MyNat data type used by C.run
intToNat :: Int -> C.MyNat
intToNat n | n <= 0 = C.Zero
           | otherwise = C.Suc (intToNat (n-1))

main :: IO ()
-- main = print $ take 100 (Writer.output (C.run CWriter.example (intToNat 100)))
main = ungetChanDeadlock
-- main = MVarDeadlock

-- Using MVars in the wrong order can result in a deadlock.
-- In this case, two threads are trying to retrieve an empty MVar, and thus waiting for each other to write to it.
-- Obvious example.
MVarDeadlock :: IO ()
MVarDeadlock = C.run (do
                a <- MVar.newEmptyMVar
                b <- MVar.newEmptyMVar
                C.fork (do
                            MVar.takeMVar a
                            MVar.writeMVar b ())
                C.fork (do
                            MVar.takeMVar b
                            MVar.writeMVar a ())
                ) (intToNat 9223372036854775800)

-- The ungetChan function can result in a deadlock, when another thread is already waiting to read from an empty channel.
ungetChanDeadlock :: IO ()
ungetChanDeadlock = C.run (do
                            chan <- Channel.newChan
                            val <- Channel.readChan chan
                            Channel.ungetChan chan 5
                            return val) (intToNat 9223372036854775800)