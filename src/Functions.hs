{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- Functions
-}

module Functions
    (
        addition,
        --substactionTree,
        --multiplicationTree,
        --divisionTree,
        --moduloTree
    ) where

import Types
import Errors
import Defines

-- Compute a "+ - div * mod" list, using defines if needed

addition :: Env -> [Tree] -> (Env, Maybe Result)
addition env [Number a, Number b] = (env, Just (Number (a + b)))
addition env [Number a, Symbol b]
    | (_, Just (Number symbolValue)) <- getSymbolValue env b =
        (env, Just (Number (a + symbolValue)))
    | otherwise = (registerError env "Symbol not found", Nothing)
addition env [Symbol a, Number b]
    | (_, Just (Number symbolValue)) <- getSymbolValue env a =
        (env, Just (Number (symbolValue + b)))
    | otherwise = (registerError env "Symbol not found", Nothing)
addition env [Symbol a, Symbol b]
    | (_, Just (Number symbolValueA)) <- getSymbolValue env a
    , (_, Just (Number symbolValueB)) <- getSymbolValue env b =
        (env, Just (Number (symbolValueA + symbolValueB)))
    | otherwise = (registerError env "Symbol not found", Nothing)
addition env list
    | length list /= 2 = (registerError env "Addition need 2 params", Nothing)
    | otherwise = (registerError env "Bad types in addition", Nothing)
