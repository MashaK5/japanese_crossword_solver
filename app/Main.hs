module Main where

import Solver
import Picosat (Solution(Unsatisfiable))
import Data.Maybe (isNothing)
import Text.Read (readMaybe, readEither)
import System.Exit
import Control.Monad


main :: IO ()
main = do
    putStr "Enter the number of rows:\n"
    (numberOfRows, argsRows) <- readInputBlock
    when (numberOfRows == 0)
        (putStr "Invalid rows input! All elements must be natural numbers separated by spaces.\n" >> 
        exitWith ExitSuccess)  

    putStr "Enter the number of columns:\n"
    (numberOfCols, argsCols) <- readInputBlock
    when (numberOfRows == 0)
        (putStr "Invalid columns input! All elements must be natural numbers separated by spaces.\n" >> 
        exitWith ExitSuccess)  

    let countLongLine maxLen args = length $ filter (> maxLen) (map (foldl (\sum cur -> sum + cur + 1) (-1)) args)  
    when (countLongLine numberOfCols argsRows /= 0)
        (putStr "Invalid rows input! Some line is too long.\n" >> 
        exitWith ExitSuccess)     
    when (countLongLine numberOfRows argsCols /= 0)
        (putStr "Invalid columns input! Some line is too long.\n" >> 
        exitWith ExitSuccess)

    putStr $ show argsRows ++ "\n"
    putStr $ show argsCols ++ "\n"


readInputBlock :: IO (Int, [[Int]])
readInputBlock = do
    n <- getLine
    if isNothing (readMaybe n :: Maybe Int) || read n < 1 
        then return (0, [[]])
    else do
        putStr "Enter the specified number of lines:\n"
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


