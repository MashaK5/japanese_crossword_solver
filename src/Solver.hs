{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Solver where

import Picosat
import NF



-- generateSAT :: Int -> Int -> [[Int]] -> [[Int]] -> [[Int]]
-- generateSAT n m list_lines list_columns = TODO()

generateSatLines' :: Int -> Int -> [[Int]] -> Int -> Formula
generateSatLines' n m lines n'
    | n' > n = Atom T 
    | otherwise = generateSatLine m n' 0 m (head lines) :/\
                 generateSatLines' n m (tail lines) (n' + 1) 

generateSatLines :: Int -> Int -> [[Int]] -> Formula
generateSatLines n m lines =  generateSatLines' n m lines 1
    

-- Эта функция по номеру строки, и списку кроссворда для этой строки
-- возвращает блок формулы для этой строки (skip_n = 0 по-умолчанию). 
generateSatLine :: Int -> Int -> Int -> Int -> [Int] -> Formula
generateSatLine m num_line skip_n n line
    | null line = paint num_line m (skip_n + 1) n 0
    | n < head line = Atom F
    | n == head line && null (tail line) = paint_amount
    | n == head line = Atom F
    | otherwise =   (paint_amount :/\
                    paint m num_line (skip_n + 1 + head line) 1 0 :/\
                    generateSatLine m num_line (skip_n + head line + 1) (n - 1 - head line) (tail line)) :\/
                    (paint m num_line (skip_n + 1) 1 0 :/\
                    generateSatLine m num_line (skip_n + 1) (n - 1) line)
        where paint_amount = paint m num_line (skip_n + 1) (head line) 1


-- эта функция по m, номеру линии, откуда и сколько клеток, какого типо покраски,
-- возвращает формулу для этого куска клеток
paint :: Int -> Int -> Int -> Int -> Int -> Formula
paint m line cell_number amount cell_type 
    | amount == 0 = Atom T
    | cell_type == 1 = new :/\ next
    | otherwise = Not new :/\ next
    where next = paint m line (cell_number + 1) (amount - 1) cell_type;
            new = getAtomF m line cell_number


-- Эта функция по m, строке и столбцу возвращает переменную для Formula.
-- Нумерация клеток как у матриц. Пример для m = 3:
-- 1 2 3
-- 4 5 6
-- 7 8 9
getAtomF :: Int -> Int -> Int -> Formula 
getAtomF m line column = Atom $ Var $ (line - 1) * m + column

-- -- Этот блок функций трансформирует Formula (NF) в [[Int]] (формат для солвера).
-- -- Сначала переводит формулу в КНФ.
-- formulaToList :: Formula -> [[Int]]
-- formulaToList f = parserC $ cnf f

-- -- Эта функция парсит КНФ, создавая список для каждого дизъюнкта 
-- -- и объединяя их потом в список списков.
-- parserC :: Formula -> [[Int]]
-- parserC (f1 :* f2)   = parserC f1 ++ parserC f2
-- parserC f@(f1 :+ f2) = [parserD f]
-- parserC f = [[1]]

-- -- Эта функция парсит дизъюнкт.
-- parserD :: Formula -> [Int]
-- parserD (f1 :+ f2) = parserD f1 ++ parserD f2
-- parserD (Not (Atom (Var i))) = [-i]
-- parserD (Atom (Var i)) = [i]


-- -- Эта функция по n, строке и столбцу возвращает переменную для Formula.
-- -- Нумерация клеток как у матриц. Пример для n = 3:
-- -- 1 2 3
-- -- 4 5 6
-- -- 7 8 9
-- genAtomF :: Int -> Int -> Int -> Formula
-- genAtomF n r c = Atom $ Var $ (r - 1) * n + c



-- -- Этот блок функций с помощью вызываемых функций генерирует 4 КНФ:
-- -- правила для строк, для столбцов и для обоих видов диагоналей.
-- -- Потом преобразует их в [[Int]] (формат для солвера) и запускает солвер.
-- process :: Int -> IO Solution
-- process n = solve $ generateSAT n
