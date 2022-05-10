module Main where

import Solver
import Picosat (Solution(Unsatisfiable))
import Data.Maybe (isNothing)
import Text.Read (readMaybe, readEither)


main :: IO ()
main = do
    n <- getLine
    if isNothing (readMaybe n :: Maybe Int) || read n < 1 
        then putStr "'n' must be a natural number!\n"
    else do   
        args <- readArgs (read n) []
        let res = reverse args
        putStr $ show res ++ "\n"


readArgs :: Int -> [[Int]] -> IO [[Int]]
readArgs 0 xs = do
    return xs
readArgs n xs = do
    lineArgs <- readLineArgs
    readArgs (n - 1) (lineArgs : xs) 


readLineArgs :: IO [Int]
readLineArgs = do
    s <- getLine
    return $ map read $ words s

