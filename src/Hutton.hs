module Hutton where

data Expr = Lit Integer | Add Expr Expr

-- 1

eval :: Expr -> Integer
eval (Lit x)   = x
eval (Add x y) = eval x + eval y

-- 2

printExpr :: Expr -> String
printExpr (Lit x)   = show x
printExpr (Add x y) = printExpr x ++ " + " ++ printExpr y
