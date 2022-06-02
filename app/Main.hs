module Main where

import Solver
import Picosat (Solution(Unsatisfiable))
import Data.Maybe (isNothing)
import Text.Read (readMaybe, readEither)



main :: IO ()
main = do
    (numberOfRows, argsRows) <- readInputBlock
    if numberOfRows == 0
        then putStr "Invalid rows input! All elements must be natural numbers separated by spaces.\n"
    else do     
        (numberOfCols, argsCols) <- readInputBlock
        if numberOfRows == 0
            then putStr "Invalid columns input! All elements must be natural numbers separated by spaces.\n"    
        else do    
            putStr $ show argsRows ++ "\n"
            putStr $ show argsCols ++ "\n"


readInputBlock :: IO (Int, [[Int]])
readInputBlock = do
    n <- getLine
    if isNothing (readMaybe n :: Maybe Int) || read n < 1 
        then return (0, [[]])
    else do
        revArgs <- readArgs (read n) []
        let args = reverse revArgs
        return (read n, args)


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


