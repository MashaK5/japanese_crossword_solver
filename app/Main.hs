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
        if numberOfCols == 0
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
        return $ checkAllNums (read n) args


checkAllNums :: Int -> [[String]] -> (Int, [[Int]])
checkAllNums n args | haveInvalidArgs = (0, [[]])
                    | otherwise = (n, transformArgs args)
    where haveInvalidArgs = length (filter (/= 0) (map (length . filter (== 0)) (transformArgs args))) /= 0               
          transformArgs args = map (map (\x -> if isNothing (readMaybe x :: Maybe Int) || read x < 1 then 0 else read x)) args


readArgs :: Int -> [[String]] -> IO [[String]]
readArgs 0 xs = do
    return xs
readArgs n xs = do
    lineArgs <- readLineArgs
    readArgs (n - 1) (lineArgs : xs) 


readLineArgs :: IO [String]
readLineArgs = do
    s <- getLine
    return $ words s


