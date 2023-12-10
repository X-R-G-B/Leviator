{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- Defines
-}

module Defines
    (
        Define (Define),
        Error (Error),
        Env (Env, defines, errors),
        registerDefine,
        getSymbolValue
    ) where

import AST
import Data.Int (Int64)

-- Define = <SYMBOL> <EXPRESSION>
data Define = Define {
    symbol :: String,
    expression :: Tree
} deriving (Show)

-- Error = <FILE> <MESSAGE>
data Error = Error {
    file :: String,
    message :: String
} deriving (Show)

-- used to store defines, and more in the future
data Env = Env {
    defines :: [Define],
    errors :: [Error]
} deriving (Show)

-- TODO: Handle case where the define is a lambda / not defined
getSymbolValue :: Env -> String -> Int64
getSymbolValue (Env { defines = [], errors = _ }) _ = 0
getSymbolValue (Env { defines = (Define symbl value):rest, errors = _ }) symbolToFind
    | symbl == symbolToFind = case value of
        (Number number) -> number
        (Boolean True) -> 1
        (Boolean False) -> 0
        (Symbol _) -> 0
    | otherwise = getSymbolValue (Env { defines = rest, errors = [] }) symbolToFind

-- Register a define in the Defines list
registerDefine :: Env -> Symbol -> Tree -> Env
registerDefine env symbol value =
    Env (defines env ++ [Define symbol value]) (errors env)
