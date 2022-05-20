import qualified Data.C as C
import qualified Data.Writer as Writer
import qualified Data.CWriter as CWriter

-- To convert regular integers to the MyNat data type used by C.run
intToNat :: Int -> C.MyNat
intToNat n | n <= 0 = C.Zero
           | otherwise = C.Suc (intToNat (n-1))

main :: IO ()
main = print $ take 100 (Writer.output (C.run CWriter.example (intToNat 100)))