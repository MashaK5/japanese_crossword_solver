{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Solver where

import Picosat
import NF


-- Этот блок функций с помощью вызываемых функций генерирует 4 КНФ:
-- правила для строк, для столбцов и для обоих видов диагоналей.
-- Потом преобразует их в [[Int]] (формат для солвера) и запускает солвер.
process :: Int -> IO Solution
process n = solve $ generateSAT n

generateSAT :: Int -> [[Int]]
generateSAT n = TODO()


-- Этот блок функций трансформирует Formula (NF) в [[Int]] (формат для солвера).
-- Сначала переводит формулу в КНФ.
formulaToList :: Formula -> [[Int]]
formulaToList f = parserC $ cnf f

-- Эта функция парсит КНФ, создавая список для каждого дизъюнкта 
-- и объединяя их потом в список списков.
parserC :: Formula -> [[Int]]
parserC (f1 :* f2)   = parserC f1 ++ parserC f2
parserC f@(f1 :+ f2) = [parserD f]
parserC f = [[1]]

-- Эта функция парсит дизъюнкт.
parserD :: Formula -> [Int]
parserD (f1 :+ f2) = parserD f1 ++ parserD f2
parserD (Not (Atom (Var i))) = [-i]
parserD (Atom (Var i)) = [i]


-- Эта функция по n, строке и столбцу возвращает переменную для Formula.
-- Нумерация клеток как у матриц. Пример для n = 3:
-- 1 2 3
-- 4 5 6
-- 7 8 9
genAtomF :: Int -> Int -> Int -> Formula
genAtomF n r c = Atom $ Var $ (r - 1) * n + c





