module Expr
    ( Expr(..)
    , Env
    , eval
    , simplify
    , evalBatch
    , countResults
    ) where

-- Expression language
data Expr
    = Lit Double
    | Var String
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Let String Expr Expr
    deriving (Show, Eq)

-- Variable environment
type Env = [(String, Double)]


-- =========================================================
--  EVALUATION
-- =========================================================

eval :: Env -> Expr -> Either String Double

-- Number
eval _ (Lit n) =
    Right n

-- Variable
eval env (Var name) =
    case lookup name env of
        Just value -> Right value
        Nothing    -> Left ("Undefined variable: " ++ name)

-- Addition
eval env (Add e1 e2) =
    case eval env e1 of
        Left err -> Left err
        Right x ->
            case eval env e2 of
                Left err -> Left err
                Right y -> Right (x + y)

-- Subtraction
eval env (Sub e1 e2) =
    case eval env e1 of
        Left err -> Left err
        Right x ->
            case eval env e2 of
                Left err -> Left err
                Right y -> Right (x - y)

-- Multiplication
eval env (Mul e1 e2) =
    case eval env e1 of
        Left err -> Left err
        Right x ->
            case eval env e2 of
                Left err -> Left err
                Right y -> Right (x * y)

-- Division
eval env (Div e1 e2) =
    case eval env e1 of
        Left err -> Left err
        Right x ->
            case eval env e2 of
                Left err -> Left err
                Right y ->
                    if y == 0
                        then Left "Division by zero"
                        else Right (x / y)

-- Let binding
eval env (Let name valueExpr bodyExpr) =
    case eval env valueExpr of
        Left err -> Left err
        Right value ->
            eval ((name, value) : env) bodyExpr


-- =========================================================
-- STEP 7: SIMPLIFICATION
-- =========================================================

simplify :: Expr -> Expr

simplify (Lit n) =
    Lit n

simplify (Var name) =
    Var name

simplify (Add e1 e2) =
    case (simplify e1, simplify e2) of
        (x, Lit 0) -> x
        (Lit 0, y) -> y
        (x, y)     -> Add x y

simplify (Sub e1 e2) =
    case (simplify e1, simplify e2) of
        (x, Lit 0) -> x
        (x, y)     -> Sub x y

simplify (Mul e1 e2) =
    case (simplify e1, simplify e2) of
        (x, Lit 1) -> x
        (Lit 1, y) -> y
        (x, y)     -> Mul x y

simplify (Div e1 e2) =
    case (simplify e1, simplify e2) of
        (x, Lit 1) -> x
        (x, y)     -> Div x y

simplify (Let name valueExpr bodyExpr) =
    Let name (simplify valueExpr) (simplify bodyExpr)


-- =========================================================
-- STEP 8: HIGHER-ORDER FUNCTION + CURRYING
-- =========================================================

-- Evaluate many expressions and keep only successful results.
-- (eval env) is a partially applied function.
evalBatch :: Env -> [Expr] -> [Double]
evalBatch env expressions =
    [value | Right value <- map (eval env) expressions]


-- Count successful and failed evaluations.
-- Result: (number of successes, number of failures)
countResults :: Env -> [Expr] -> (Int, Int)
countResults env expressions =
    foldr countOne (0, 0) (map (eval env) expressions)
  where
    countOne result (successes, failures) =
        case result of
            Right _ -> (successes + 1, failures)
            Left _  -> (successes, failures + 1)