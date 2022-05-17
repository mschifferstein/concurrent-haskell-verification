import qualified Data.C as C
import qualified Data.Writer as Writer
import qualified Data.CWriter as CWriter

main :: IO ()
main = print $ take 100 (Writer.output (C.run $ CWriter.example))