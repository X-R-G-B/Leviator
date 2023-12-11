{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- Functions
-}

module Functions
    (
        addition,
        subtraction,
        multiplication,
        division,
        modulo
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

multiplication :: Env -> [Tree] -> (Env, Maybe Result)
multiplication env [Number a, Number b] = (env, Just (Number (a * b)))
multiplication env [Number a, Symbol b]
    | (_, Just (Number symbolValue)) <- getSymbolValue env b =
        (env, Just (Number (a * symbolValue)))
    | otherwise = (registerError env "Symbol not found", Nothing)
multiplication env [Symbol a, Number b]
    | (_, Just (Number symbolValue)) <- getSymbolValue env a =
        (env, Just (Number (symbolValue * b)))
    | otherwise = (registerError env "Symbol not found", Nothing)
multiplication env [Symbol a, Symbol b]
    | (_, Just (Number symbolValueA)) <- getSymbolValue env a
    , (_, Just (Number symbolValueB)) <- getSymbolValue env b =
        (env, Just (Number (symbolValueA * symbolValueB)))
    | otherwise = (registerError env "Symbol not found", Nothing)
multiplication env list
    | length list /= 2 = (registerError env "* need 2 params", Nothing)
    | otherwise = (registerError env "Bad types in multiplication", Nothing)

subtraction :: Env -> [Tree] -> (Env, Maybe Result)
subtraction env [Number a, Number b] = (env, Just (Number (a - b)))
subtraction env [Number a, Symbol b]
    | (_, Just (Number symbolValue)) <- getSymbolValue env b =
        (env, Just (Number (a - symbolValue)))
    | otherwise = (registerError env "Symbol not found", Nothing)
subtraction env [Symbol a, Number b]
    | (_, Just (Number symbolValue)) <- getSymbolValue env a =
        (env, Just (Number (symbolValue - b)))
    | otherwise = (registerError env "Symbol not found", Nothing)
subtraction env [Symbol a, Symbol b]
    | (_, Just (Number symbolValueA)) <- getSymbolValue env a
    , (_, Just (Number symbolValueB)) <- getSymbolValue env b =
        (env, Just (Number (symbolValueA - symbolValueB)))
    | otherwise = (registerError env "Symbol not found", Nothing)
subtraction env list
    | length list /= 2 = (registerError env "- need 2 params", Nothing)
    | otherwise = (registerError env "Bad types in subtraction", Nothing)

division :: Env -> [Tree] -> (Env, Maybe Result)
division env [Number a, Number b]
    | b == 0 = (registerError env "Division by 0", Nothing)
    | otherwise = (env, Just (Number (a `div` b)))
division env [Symbol a, Number b]
    | b == 0 = (registerError env "Division by 0", Nothing)
    | (_, Just (Number symbolValue)) <- getSymbolValue env a =
        (env, Just (Number (symbolValue `div` b)))
    | otherwise = (registerError env "Symbol not found", Nothing)
division env [Number a, Symbol b]
    | (_, Just (Number symbolValue)) <- getSymbolValue env b
    , symbolValue == 0 = (registerError env "Division by 0", Nothing)
    | (_, Just (Number symbolValue)) <- getSymbolValue env b =
        (env, Just (Number (a `div` symbolValue)))
    | otherwise = (registerError env "Symbol not found", Nothing)
division env [Symbol a, Symbol b]
    | (_, Just (Number _)) <- getSymbolValue env a
    , (_, Just (Number symbolValueB)) <- getSymbolValue env b
    , symbolValueB == 0 = (registerError env "Division by 0", Nothing)
    | (_, Just (Number symbolValueA)) <- getSymbolValue env a
    , (_, Just (Number symbolValueB)) <- getSymbolValue env b =
        (env, Just (Number (symbolValueA `div` symbolValueB)))
    | otherwise = (registerError env "Symbol not found", Nothing)
division env list
    | length list /= 2 = (registerError env "/ need 2 params", Nothing)
    | otherwise = (registerError env "Bad types in division", Nothing)

modulo :: Env -> [Tree] -> (Env, Maybe Result)
modulo env [Number a, Number b]
    | b == 0 = (registerError env "Modulo by 0", Nothing)
    | otherwise = (env, Just (Number (a `mod` b)))
modulo env [Symbol a, Number b]
    | b == 0 = (registerError env "Modulo by 0", Nothing)
    | (_, Just (Number symbolValue)) <- getSymbolValue env a =
        (env, Just (Number (symbolValue `mod` b)))
    | otherwise = (registerError env "Symbol not found", Nothing)
modulo env [Number a, Symbol b]
    | (_, Just (Number symbolValue)) <- getSymbolValue env b
    , symbolValue == 0 = (registerError env "Modulo by 0", Nothing)
    | (_, Just (Number symbolValue)) <- getSymbolValue env b =
        (env, Just (Number (a `mod` symbolValue)))
    | otherwise = (registerError env "Symbol not found", Nothing)
modulo env [Symbol a, Symbol b]
    | (_, Just (Number _)) <- getSymbolValue env a
    , (_, Just (Number symbolValueB)) <- getSymbolValue env b
    , symbolValueB == 0 = (registerError env "Modulo by 0", Nothing)
    | (_, Just (Number symbolValueA)) <- getSymbolValue env a
    , (_, Just (Number symbolValueB)) <- getSymbolValue env b =
        (env, Just (Number (symbolValueA `mod` symbolValueB)))
    | otherwise = (registerError env "Symbol not found", Nothing)
modulo env list
    | length list /= 2 = (registerError env "% need 2 params", Nothing)
    | otherwise = (registerError env "Bad types in modulo", Nothing)
