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
    when (numberOfRows == -1)
        (putStr "Invalid rows input! There is a string of several numbers with zero element.\n" >> 
        exitWith ExitSuccess)
    when (numberOfRows == -2)
        (putStr "Invalid rows input! There is an empty line of rules.\n" >> 
        exitWith ExitSuccess)           

    putStr "Enter the number of columns:\n"
    (numberOfCols, argsCols) <- readInputBlock
    when (numberOfCols == 0)
        (putStr "Invalid columns input! All elements must be natural numbers separated by spaces.\n" >> 
        exitWith ExitSuccess)
    when (numberOfCols == -1)
        (putStr "Invalid columns input! There is a string of several numbers with zero element.\n" >> 
        exitWith ExitSuccess) 
    when (numberOfCols == -2)
        (putStr "Invalid columns input! There is an empty line of rules.\n" >> 
        exitWith ExitSuccess)          

    let countLongLine maxLen args = length $ filter (> maxLen) (map (foldl (\sum cur -> sum + cur + 1) (-1)) args)  
    when (countLongLine numberOfCols argsRows /= 0)
        (putStr "Invalid rows input! Some line is too long.\n" >> 
        exitWith ExitSuccess)     
    when (countLongLine numberOfRows argsCols /= 0)
        (putStr "Invalid columns input! Some line is too long.\n" >> 
        exitWith ExitSuccess)

    --putStr $ show $ process (numberOfRows, argsRows) (numberOfCols, argsCols)

    res <- process (numberOfRows, argsRows) (numberOfCols, argsCols)
    if res == Unsatisfiable then putStr $ show res ++ "\n"
        else putStr $ printSolution numberOfRows numberOfCols res ++ "\n"


--Эта функция обрабатывает ввод условий строк/столбцов.
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


--Эта функция проверяет, что все условия корректны.
checkAllNums :: Int -> [[String]] -> (Int, [[Int]])
checkAllNums n args | haveInvalidArgs = (0, [[]])
                    | haveInvalidNulls = (-1, [[]])
                    | haveEmptyRules = (-2, [[]])
                    | otherwise = (n, transformArgs args)
    where transformArgs args = map (map (\x -> if isNothing (readMaybe x :: Maybe Int) || read x < 0 then -1 else read x)) args
          haveInvalidArgs = length (filter (/= 0) (map (length . filter (== -1)) (transformArgs args))) /= 0               
          haveInvalidNulls = length (filter (== False) (map isCorrectRules (transformArgs args))) /= 0
          isCorrectRules xs = length xs == 1 || length (filter (== 0) xs) == 0
          haveEmptyRules = length (filter (== True) (map (\xs -> length xs == 0) (transformArgs args))) /= 0


-- Эти функции считывают строчки условий.
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


