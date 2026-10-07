module Main where

import Expr


-- =========================================================
-- ENVIRONMENT
-- =========================================================

env :: Env
env =
    [ ("x", 8)
    , ("y", 4)
    ]


-- =========================================================
-- SAMPLE EXPRESSIONS
-- =========================================================

-- 6 + (2 * 5)
expr1 :: Expr
expr1 =
    Add (Lit 6) (Mul (Lit 2) (Lit 5))


-- x + (y * 2)
expr2 :: Expr
expr2 =
    Add (Var "x") (Mul (Var "y") (Lit 2))


-- let z = 10 in z - 3
expr3 :: Expr
expr3 =
    Let "z"
        (Lit 10)
        (Sub (Var "z") (Lit 3))


-- 10 / 0
expr4 :: Expr
expr4 =
    Div (Lit 10) (Lit 0)


-- unknown + 1
expr5 :: Expr
expr5 =
    Add (Var "unknown") (Lit 1)


-- (x + 0) * 1
expr6 :: Expr
expr6 =
    Mul
        (Add (Var "x") (Lit 0))
        (Lit 1)


-- =========================================================
-- MAIN
-- =========================================================

main :: IO ()
main = do

    putStrLn "EC8206 Functional Programming Project"
    putStrLn "--------------------------------------"

    putStrLn "\nSample 1:"
    print expr1
    print (eval env expr1)

    putStrLn "\nSample 2:"
    print expr2
    print (eval env expr2)

    putStrLn "\nSample 3:"
    print expr3
    print (eval env expr3)

    putStrLn "\nSample 4:"
    print expr4
    print (eval env expr4)

    putStrLn "\nSample 5:"
    print expr5
    print (eval env expr5)

    putStrLn "\nSimplification:"
    print expr6
    print (simplify expr6)
    print (eval env (simplify expr6))

    putStrLn "\nBatch evaluation:"
    print (evalBatch env [expr1, expr2, expr3, expr4, expr5])

    putStrLn "\nSuccess / failure count:"
    print (countResults env [expr1, expr2, expr3, expr4, expr5])