module Computing.Operators.Calculate
  (
    addition,
    subtraction,
    multiplication,
    division,
    modulo,
  ) where

import Computing.Operators.EvaluateSymbol
import Types
import Data.Int (Int64)
import Computing.Errors

-- Compute a "+ - div * mod" list, using defines if needed

calculate :: Int64 -> Int64 -> Symbol -> Tree
calculate a b "+" = Number (a + b)
calculate a b "-" = Number (a - b)
calculate a b "*" = Number (a * b)
calculate a b "div" = Number (a `div` b)
calculate a b "mod" = Number (a `mod` b)
calculate _ _ _ = Number 0

maybeCalculate :: Maybe Tree -> Maybe Tree -> Symbol -> Env -> (Env, Result)
maybeCalculate (Just (Number a)) (Just (Number b)) operator env = (env, Left (Just (calculate a b operator)))
maybeCalculate _ _ _ env = (registerError env "Symbol not found", Right (undefined))

calculateOperator :: Env -> [Tree] -> Symbol -> (Env, Result)
calculateOperator env [Number a, Number b] operator =
  (env, Left (Just (calculate a b operator)))
calculateOperator env [Number a, Symbol b] operator =
  maybeCalculate (Just (Number a)) (evaluateSymbol env b) operator env
calculateOperator env [Symbol a, Number b] operator =
  maybeCalculate (evaluateSymbol env a) (Just (Number b)) operator env
calculateOperator env [Symbol a, Symbol b] operator =
  maybeCalculate (evaluateSymbol env a) (evaluateSymbol env b) operator env
calculateOperator env list _
    | length list /= 2 = (registerError env "Addition need 2 params", Right (undefined))
    | otherwise = (registerError env "Bad types in addition", Right (undefined))

addition :: Env -> [Tree] -> (Env, Result)
addition env trees = calculateOperator env trees "+"

subtraction :: Env -> [Tree] -> (Env, Result)
subtraction env trees = calculateOperator env trees "-"

multiplication :: Env -> [Tree] -> (Env, Result)
multiplication env trees = calculateOperator env trees "*"

division :: Env -> [Tree] -> (Env, Result)
division env trees = calculateOperator env trees "div"

modulo :: Env -> [Tree] -> (Env, Result)
modulo env trees = calculateOperator env trees "mod"
