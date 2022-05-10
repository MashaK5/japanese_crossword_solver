module NF where


type Symb = Int    -- символ переменной

infixl 2 :<=>         -- эквивалентность
infixl 3 :=>          -- импликация
infixl 4 :+           -- дизъюнкция
infixl 5 :*           -- конъюнкция

data Atom = Var Symb | T | F       -- атомарные формулы 
    deriving (Show, Eq)

data Formula = Atom Atom |         -- формулы
               Not Formula |
               Formula :* Formula |
               Formula :+ Formula |
               Formula :=> Formula |
               Formula :<=> Formula
    deriving (Eq)


-- Реализация вывода формул. 
-- Вывод будет выглядеть более красиво для формул без импликации и эквивалетности 
-- (он в первую овередь для таких формул).
show' :: Bool -> Formula -> String
show' beforeCon (f1 :+ f2) | not beforeCon = show' beforeCon f1 ++ " :+ " ++ show' beforeCon f2
                           | otherwise  = "(" ++ show' False  f1 ++ " :+ " ++ show' False f2 ++ ")"
show' beforeCon (f1 :* f2) = show' True f1 ++ " :* " ++ show' True f2
show' beforeCon (Not f) | Atom x <- f = "Not " ++ show f
                        | otherwise   = "Not (" ++ show f ++ ")"
show' beforeCon (Atom (Var x)) = show x
show' beforeCon (Atom T) = "tr"
show' beforeCon (Atom F) = "fal"
show' beforeCon (f1 :=> f2) = "(" ++ show' False  f1 ++ " :=> " ++ show' False f2 ++ ")"
show' beforeCon (f1 :<=> f2) = "(" ++ show' False  f1 ++ " :<=> " ++ show' False f2 ++ ")"

instance Show Formula where
    show f = show' False f
    

-- Упрощение вида формулы, связками могут быть только /\, \/, ~.
-- Работает линейно, так как проходит каждую связку формулы один раз.
simpl :: Formula -> Formula
simpl (f1 :<=> f2) = simpl (f1 :=> f2) :* simpl (f2 :=> f1)
simpl (f1 :=> f2) = Not (simpl f1) :+ simpl f2
simpl (f1 :+ f2) = simpl f1 :+ simpl f2
simpl (f1 :* f2) = simpl f1 :* simpl f2
simpl (Not f) = Not $ simpl f
simpl a = a


-- Реализация приведения к ННФ.
-- Работает линейно, так как проходит каждую связку формулы один раз.
nnf' :: Formula -> Formula
nnf' (f1 :+ f2) = nnf' f1 :+ nnf' f2
nnf' (f1 :* f2) = nnf' f1 :* nnf' f2
nnf' (Not f) | Atom T       <- f = Atom F
             | Atom F       <- f = Atom T
             | (Not f1)     <- f = nnf' f1
             | (f1 :+ f2)   <- f = nnf' (Not f1) :* nnf' (Not f2)
             | (f1 :* f2)   <- f = nnf' (Not f1) :+ nnf' (Not f2)
             | otherwise         = Not f
nnf' x = x

nnf :: Formula -> Formula
nnf f = nnf' $ simpl f


-- Проверка наличия дизъюнкций в формуле. 
-- Будет применяться при построении ДНФ.
haveDis :: Formula -> Bool
haveDis (f1 :* f2) = haveDis f1 || haveDis f2 
haveDis (f1 :+ f2) = True 
haveDis x = False

-- Реализация приведения к ДНФ.
dnf' :: Formula -> Formula
dnf' (f1 :+ f2) = dnf' f1 :+ dnf' f2
dnf' f@(f1 :* f2) | (f3 :+ f4) <- f1 = dnf' (f3 :* f2) :+ dnf' (f4 :* f2)
                  | (f3 :+ f4) <- f2 = dnf' (f1 :* f3) :+ dnf' (f1 :* f4)
                  | haveDis f        = dnf' (dnf' f1 :* dnf' f2)
                  | otherwise        = f
dnf' f = f

dnf :: Formula -> Formula
dnf f = dnf' $ nnf f


-- Проверка наличия конъюнкций в формуле. 
-- Будет применяться при построении КНФ.
haveCon :: Formula -> Bool
haveCon (f1 :+ f2) = haveCon f1 || haveCon f2 
haveCon (f1 :* f2) = True 
haveCon x = False

-- Реализация приведения к КНФ.
cnf' :: Formula -> Formula
cnf' (f1 :* f2) = cnf' f1 :* cnf' f2
cnf' f@(f1 :+ f2) | (f3 :* f4) <- f1 = cnf' (f3 :+ f2) :* cnf' (f4 :+ f2)
                  | (f3 :* f4) <- f2 = cnf' (f1 :+ f3) :* cnf' (f1 :+ f4)
                  | haveCon f        = cnf' (cnf' f1 :+ cnf' f2)
                  | otherwise        = f
cnf' f = f

cnf :: Formula -> Formula
cnf f = cnf' $ nnf f


