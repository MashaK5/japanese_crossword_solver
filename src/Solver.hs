{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Solver where

import Picosat
import NF


------------------------ Блок про вывод формулы --------------

data TypeLine = Row | Column
    deriving (Eq)


generateSat :: Int -> Int -> [[Int]] -> [[Int]] -> Formula
generateSat n m list_rows list_columns = 
    generateSatLines n m list_rows Row :/\
    generateSatLines m n list_columns Column



generateSatLines' :: Int -> Int -> Int -> [[Int]] -> TypeLine -> Formula
generateSatLines' n' n m lines type_line
    | n' > n = Atom T
    | otherwise = generateSatLine n m n' 0 m (head lines) type_line :/\
                 generateSatLines'  (n' + 1) n m (tail lines) type_line

generateSatLines :: Int -> Int -> [[Int]] -> TypeLine -> Formula
generateSatLines = generateSatLines' 1


-- Эта функция по номеру строки, и списку кроссворда для этой строки
-- возвращает блок формулы для этой строки (skip_n = 0 по-умолчанию). 
generateSatLine :: Int -> Int -> Int -> Int -> Int -> [Int] -> TypeLine -> Formula
generateSatLine n m num_line skip_line onward_line line type_line
    | null line = not_paint_amount
    | head line == 0 = not_paint_amount
    | onward_line < head line = Atom F
    | onward_line == head line && null (tail line) = paint_amount
    | onward_line == head line = Atom F
    | otherwise =   (paint_amount :/\
                    paint n m num_line (skip_line + 1 + head line) 1 0 type_line :/\
                    generateSatLine n m num_line (skip_line + head line + 1) (onward_line - 1 - head line) (tail line) type_line) :\/
                    (paint n m num_line (skip_line + 1) 1 0 type_line :/\
                    generateSatLine n m num_line (skip_line + 1) (onward_line - 1) line type_line)
        where paint_amount = paint n m num_line (skip_line + 1) (head line) 1 type_line;
                not_paint_amount = paint n m num_line (skip_line + 1) onward_line 0 type_line
            


-- Эта функция по m, номеру линии, откуда и сколько клеток, какого типо покраски,
-- возвращает формулу для этого куска клеток
paint :: Int -> Int -> Int -> Int -> Int -> Int -> TypeLine -> Formula
paint n m line cell_number amount cell_type type_line
    | amount == 0 = Atom T
    | cell_type == 1 = new :/\ next
    | otherwise = Not new :/\ next
    where next = paint n m line (cell_number + 1) (amount - 1) cell_type type_line;
            new = getAtomF' n m line cell_number type_line


getAtomF' :: Int -> Int -> Int -> Int -> TypeLine -> Formula 
getAtomF' n m row column type_line 
        | type_line == Row = getAtomF m row column
        | otherwise = getAtomF n column row

-- Эта функция по n, m, строке и столбцу возвращает переменную для Formula.
-- Нумерация клеток как у матриц. Пример для n = 2, m = 3:
-- 1 2 3
-- 4 5 6
getAtomF :: Int -> Int -> Int -> Formula
getAtomF m row column = Atom $ Var $ (row - 1) * m + column



------------------------ Блок про перевод Formula -> [[Int]] --------

-- Этот блок функций трансформирует Formula (NF) в [[Int]] (формат для солвера).
-- Сначала переводит формулу в КНФ.
formulaToList :: Formula -> [[Int]]
formulaToList f = parserC $ cnf f

-- Эта функция парсит КНФ, создавая список для каждого дизъюнкта 
-- и объединяя их потом в список списков.
parserC :: Formula -> [[Int]]
parserC (f1 :/\ f2)   = parserC f1 ++ parserC f2
parserC f@(f1 :\/ f2) = [parserD f]
parserC f = [[1]]

-- Эта функция парсит дизъюнкт.
parserD :: Formula -> [Int]
parserD (f1 :\/ f2) = parserD f1 ++ parserD f2
parserD (Not (Atom (Var i))) = [-i]
parserD (Atom (Var i)) = [i]




------------------------ Блок про запуск SAT-solver --------

-- Этот блок функций с помощью вызываемых функций генерирует 4 КНФ:
-- правила для строк, для столбцов и для обоих видов диагоналей.
-- Потом преобразует их в [[Int]] (формат для солвера) и запускает солвер.
process :: (Int, [[Int]]) -> (Int, [[Int]]) -> IO Solution
process (n, list_rows) (m, list_columns) = 
    solve $ formulaToList $ cnf $ generateSat n m list_rows list_columns


----------------------- Блок о вывода ----------------------

wordSolutionLen = 9

printSolution :: Int -> Int -> Solution -> String
printSolution n m sol = toTable n m $ read $ drop wordSolutionLen (show sol)

toTable :: Int -> Int -> [Int] -> String
toTable n pos [] = "\n"
toTable n 0 xs = '\n' : toTable n n xs
toTable n pos (x:xs) | x < 0 = '0' : ' ' : toTable n (pos - 1) xs
                     | x > 0 = '+' : ' ' : toTable n (pos - 1) xs



