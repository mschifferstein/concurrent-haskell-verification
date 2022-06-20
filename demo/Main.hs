import qualified Programs

main :: IO ()
-- main = print $ "This program terminates: " ++ show Programs.mVarDeadlock
-- main = print $ "This program terminates: " ++ show Programs.mVarNoDeadlock
-- main = print $ "This program terminates: " ++ show Programs.simpleDeadlock
-- main = print $ "This program terminates: " ++ show Programs.simple
main = print $ "This program terminates: " ++ show Programs.failDetect