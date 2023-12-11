{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- Defines
-}

module Defines
    (
        Define (Define),
        Env (Env),
        registerDefine,
        getSymbolValue
    ) where

import AST
import Data.Int (Int64)
 
-- Define = <SYMBOL> <EXPRESSION>
data Define = Define {
    symbol :: String,
    expression :: Atom
} deriving (Show)

-- used to store defines, and more in the future
data Env = Env {
    defines :: [Define]
} deriving (Show)

-- TODO: Handle case where the define is a lambda / not defined
getSymbolValue :: Env -> String -> Int64
getSymbolValue (Env []) _ = 0
getSymbolValue (Env ((Define symbl value):rest)) symbolToFind
    | symbl == symbolToFind = case value of
        (Number number) -> number
        (Boolean True) -> 1
        (Boolean False) -> 0
        (Symbol _) -> 0
    | otherwise = getSymbolValue (Env rest) symbolToFind

-- Register a define in the Defines list
registerDefine :: Env -> Tree -> Env
registerDefine env
        (Node "define"
        (Just (Leaf (Symbol defSymbol)))
        (Just (Leaf defexpression)))
        = env { defines = defines env ++ [Define defSymbol defexpression] }
registerDefine env _ = env
