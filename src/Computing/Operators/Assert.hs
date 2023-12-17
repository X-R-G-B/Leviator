{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- Assert
-}

module Computing.Operators.Assert
    (
        equal,
        notEqual,
        inferior,
        inferiorOrEqual,
        superior,
        superiorOrEqual
    ) where

import Types
import Computing.Operators.EvaluateSymbol
import Data.Int (Int64)
import Computing.Errors

assert :: Int64 -> Int64 -> Symbol -> Tree
assert a b ">" = Boolean (a > b)
assert a b "<" = Boolean (a < b)
assert a b ">=" = Boolean (a >= b)
assert a b "<=" = Boolean (a <= b)
assert a b "eq?" = Boolean (a == b)
assert a b "diff?" = Boolean (a /= b)
assert _ _ _ = Boolean (False)

maybeAssert :: Maybe Tree -> Maybe Tree -> Symbol -> Env -> (Env, Result)
maybeAssert (Just (Number a)) (Just (Number b)) operator env =
    (env, Left (Just (assert a b operator)))
maybeAssert _ _ _ env =
    (registerError env "Symbol not found", Right (undefined))

assertOperator :: Env -> [Tree] -> Symbol -> (Env, Result)
assertOperator env [Number a, Number b] operator =
    (env, Left (Just (assert a b operator)))
assertOperator env [Number a, Symbol b] operator =
    maybeAssert (Just (Number a)) (evaluateSymbol env b) operator env
assertOperator env [Symbol a, Number b] operator =
    maybeAssert (evaluateSymbol env a) (Just (Number b)) operator env
assertOperator env [Symbol a, Symbol b] operator =
    maybeAssert (evaluateSymbol env a) (evaluateSymbol env b) operator env
assertOperator env list _
    | length list /= 2 =
        (registerError env "assert need 2 params", Right (undefined))
    | otherwise = (registerError env "Bad types in assert", Right (undefined))

equal :: Env -> [Tree] -> (Env, Result)
equal env trees = assertOperator env trees "eq?"

notEqual :: Env -> [Tree] -> (Env, Result)
notEqual env trees = assertOperator env trees "diff?"

inferior :: Env -> [Tree] -> (Env, Result)
inferior env trees = assertOperator env trees "<"

superior :: Env -> [Tree] -> (Env, Result)
superior env trees = assertOperator env trees ">"

inferiorOrEqual :: Env -> [Tree] -> (Env, Result)
inferiorOrEqual env trees = assertOperator env trees "<="

superiorOrEqual :: Env -> [Tree] -> (Env, Result)
superiorOrEqual env trees = assertOperator env trees ">="
