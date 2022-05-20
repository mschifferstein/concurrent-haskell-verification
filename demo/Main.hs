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
main = C.run (do -- this basically deadlocks :)
                chan <- Channel.newChan
                -- Channel.ungetChan chan 5
                val <- Channel.readChan chan
                Channel.ungetChan chan 5
                return val) (intToNat 9223372036854775800)