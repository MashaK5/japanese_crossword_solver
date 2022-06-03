{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Solver where

import Picosat
import NF


------------------------ Запуск SAT-solver --------

-- Этот блок функций с помощью вызываемых функций генерирует 4 КНФ:
-- правила для строк, для столбцов и для обоих видов диагоналей.
-- Потом преобразует их в [[Int]] (формат для солвера) и запускает солвер.
process :: (Int, [[Int]]) -> (Int, [[Int]]) -> IO Solution
process (n, listRows) (m, listColumns) =
    solve $ formulaToList $ cnf $ generateSat n m listRows listColumns


------------------------ Вывод формулы --------------

data TypeLine = Row | Column
    deriving (Eq)


generateSat :: Int -> Int -> [[Int]] -> [[Int]] -> Formula
generateSat n m listRows listColumns =
    generateSatLines n m listRows Row
    :/\
    generateSatLines m n listColumns Column

generateSatLines :: Int -> Int -> [[Int]] -> TypeLine -> Formula
generateSatLines = generateSatLines' 1

generateSatLines' :: Int -> Int -> Int -> [[Int]] -> TypeLine -> Formula
generateSatLines' n' n m lines typeLine
    | n' > n = Atom T
    | otherwise = generateSatLine n m n' 0 m (head lines) typeLine
                :/\
                 generateSatLines'  (n' + 1) n m (tail lines) typeLine


-- Эта функция по номеру строки, и списку кроссворда для этой строки
-- возвращает блок формулы для этой строки (skip_n = 0 по-умолчанию). 
generateSatLine :: Int -> Int -> Int -> Int -> Int -> [Int] -> TypeLine -> Formula
generateSatLine n m numLine skipLine onwardLine line typeLine
    | null line = notPaintAmount
    | head line == 0 = notPaintAmount
    | onwardLine < head line = Atom F
    | onwardLine == head line && null (tail line) = paintAmount
    | onwardLine == head line = Atom F
    | otherwise =   (
                    paintAmount
                    :/\
                    paint n m numLine (skipLine + 1 + head line) 1 0 typeLine
                    :/\
                    generateSatLine n m numLine (skipLine + head line + 1) (onwardLine - 1 - head line) (tail line) typeLine
                    )
                    :\/
                    (
                    paint n m numLine (skipLine + 1) 1 0 typeLine
                    :/\
                    generateSatLine n m numLine (skipLine + 1) (onwardLine - 1) line typeLine
                    )
        where paintAmount = paint n m numLine (skipLine + 1) (head line) 1 typeLine;
              notPaintAmount = paint n m numLine (skipLine + 1) onwardLine 0 typeLine


-- Эта функция по n, m, номеру линии, откуда и сколько клеток, какого типа покраска,
-- возвращает формулу для этого куска клеток
paint :: Int -> Int -> Int -> Int -> Int -> Int -> TypeLine -> Formula
paint n m line cellNumber amount cellType typeLine
    | amount == 0 = Atom T
    | cellType == 1 = new :/\ next
    | otherwise = Not new :/\ next
    where next = paint n m line (cellNumber + 1) (amount - 1) cellType typeLine;
          new = getAtomF' n m line cellNumber typeLine

getAtomF' :: Int -> Int -> Int -> Int -> TypeLine -> Formula
getAtomF' n m row column typeLine
        | typeLine == Row = getAtomF m row column
        | otherwise = getAtomF n column row


-- Эта функция по n, m, строке и столбцу возвращает переменную для Formula.
-- Нумерация клеток как у матриц. Пример для n = 2, m = 3:
-- 1 2 3
-- 4 5 6
getAtomF :: Int -> Int -> Int -> Formula
getAtomF m row column = Atom $ Var $ (row - 1) * m + column


------------------------ Formula -> [[Int]] ------------------

--Этот блок сокращает КНФ и переводит в вид, удобный солверу.
formulaToList :: Formula -> [[Int]]
formulaToList f = parserC f

-- Эта функция парсит КНФ, создавая список для каждого дизъюнкта 
-- и объединяя их потом в список списков.
newV = 1000000

parserC :: Formula -> [[Int]]
parserC ((Atom T) :/\ (Atom T)) = []
parserC (f1 :/\ (Atom T)) = parserC f1
parserC ((Atom T) :/\ f2) = parserC f2
parserC (f1 :/\ f2)       = parserC f1 ++ parserC f2
parserC f@(f1 :\/ f2)     | (fst $ parserD f) == False = []
                          | (snd $ parserD f) == []    = [[newV], [-newV]]
                          | otherwise                  = [snd $ parserD f]
parserC f@(Atom (Var i))  = [snd $ parserD f]
parserC f@(Not (Atom (Var i))) = [snd $ parserD f]


-- Эта функция парсит дизъюнкт.
parserD :: Formula -> (Bool, [Int])
parserD ((Atom F) :\/ (Atom F)) = (True, [])
parserD (f1 :\/ (Atom F)) = parserD f1
parserD ((Atom F) :\/ f2) = parserD f2
parserD (f1 :\/ f2) = (fst (parserD f1) && fst (parserD f2), snd (parserD f1) ++ snd (parserD f2))
parserD (Not (Atom (Var i))) = (True, [-i])
parserD (Atom (Var i)) = (True, [i])
parserD (Atom T) = (False, [0])


----------------------- Вывод ----------------------------

wordSolutionLen = 9

printSolution :: (Int, [[Int]]) -> (Int, [[Int]]) -> Solution -> String
printSolution rowsRules colsRules sol = '\n' : (toTable rowsRules colsRules $ read $ drop wordSolutionLen (show sol))


toTable :: (Int, [[Int]]) -> (Int, [[Int]]) -> [Int] -> String
toTable rowsRules colsRules sol  = (topPart (maxLenRules rowsRules) (maxLenRules colsRules) (addBeginToList (maxLenRules colsRules) colsRules) 1 1)
                                   ++
                                   betweenLine (maxLenRules rowsRules) (fst colsRules) 1
                                   ++
                                   (bottomPart (maxLenRules rowsRules) (maxLenRules colsRules) (addBeginToList (maxLenRules rowsRules) rowsRules) (fst colsRules) sol 1 1)

maxLenRules :: (Int, [[Int]]) -> Int
maxLenRules (n, listRows) = maximum $ map length listRows

addBeginToList :: Int -> (Int, [[Int]]) -> (Int, [[Int]])
addBeginToList len (count, listLines) = (count, map (\l -> addLen len (length l) l) listLines)
    where addLen len curLen listLine | len == curLen = listLine
                                     | otherwise     = (-1) : (addLen len (curLen + 1) listLine)


topPart :: Int -> Int -> (Int, [[Int]]) -> Int -> Int -> String
topPart lenRulesRows lenRulesCols (m, newListRows) curR curPos
    | curR == lenRulesCols + 1       = ""
    | curPos == lenRulesRows + m + 1 = '\n' : topPart lenRulesRows lenRulesCols (m, newListRows) (curR + 1) 1
    | curPos < lenRulesRows          = ' ' : topPart lenRulesRows lenRulesCols (m, newListRows) curR (curPos + 1)
    | curPos == lenRulesRows         = ' ' : '║' : topPart lenRulesRows lenRulesCols (m, newListRows) curR (curPos + 1)
topPart lenRulesRows lenRulesCols (m, newListRows) curR curPos
    | (newListRows !! (curPos - lenRulesRows - 1) !! (curR - 1)) == -1 = ' ' : '║' : topPart lenRulesRows lenRulesCols (m, newListRows) curR (curPos + 1)
    | otherwise                         = show (newListRows !! (curPos - lenRulesRows - 1) !! (curR - 1)) ++ ('║' : topPart lenRulesRows lenRulesCols (m, newListRows) curR (curPos + 1))


bottomPart :: Int -> Int -> (Int, [[Int]]) -> Int -> [Int] -> Int -> Int -> String
bottomPart lenRulesRows lenRulesCols (n, newListCols) m sol curR curPos
    | curR == n + 1                                                              = ""
    | curR == n &&  curPos == lenRulesRows + m + 1                               = '\n' : betweenLine lenRulesRows m 0
    | curPos == lenRulesRows + m + 1                                             = '\n' : betweenLine lenRulesRows m 1 ++ bottomPart lenRulesRows lenRulesCols (n, newListCols) m sol (curR + 1) 1
    | curPos < lenRulesRows && (newListCols !! (curR - 1) !! (curPos - 1)) == -1 = ' ' : bottomPart lenRulesRows lenRulesCols (n, newListCols) m sol curR (curPos + 1)
    | curPos < lenRulesRows && (newListCols !! (curR - 1) !! (curPos - 1)) /= -1 = (show (newListCols !! (curR - 1) !! (curPos - 1))) ++ bottomPart lenRulesRows lenRulesCols (n, newListCols) m sol curR (curPos + 1)
    | curPos == lenRulesRows                                                     = (show (newListCols !! (curR - 1) !! (curPos - 1))) ++ ('║' : bottomPart lenRulesRows lenRulesCols (n, newListCols) m sol curR (curPos + 1))
bottomPart lenRulesRows lenRulesCols (n, newListCols) m (x:xs) curR curPos 
    | x < 0 = ' ' : '║' : bottomPart lenRulesRows lenRulesCols (n, newListCols) m xs curR (curPos + 1)
    | x > 0 = '╳' : '║' : bottomPart lenRulesRows lenRulesCols (n, newListCols) m xs curR (curPos + 1)


betweenLine :: Int -> Int -> Int -> String
betweenLine (-1) 1 1 = "═╣\n"
betweenLine (-1) 0 1 = "\n" 
betweenLine 0 m flag@1 = "╬" ++ betweenLine (-1) m flag
betweenLine (-1) m flag@1 = "═╬" ++ betweenLine (-1) (m - 1) flag
betweenLine (-1) 1 0 = "═╝\n"
betweenLine (-1) 0 0 = "\n" 
betweenLine 0 m flag@0 = "╩" ++ betweenLine (-1) m flag
betweenLine (-1) m flag@0 = "═╩" ++ betweenLine (-1) (m - 1) flag
betweenLine lenRulesRows m flag = "═" ++ betweenLine (lenRulesRows - 1) m flag
