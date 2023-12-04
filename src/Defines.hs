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
    ) where

import AST
 
-- Define = <SYMBOL> <EXPRESSION>

data Define = Define {
    symbol :: String,
    expression :: Atom
} deriving (Show)

data Env = Env {
    defines :: [Define]
} deriving (Show)

-- Register a define in the Defines list
registerDefine :: Env -> Tree -> Env
registerDefine env
        (Node "define"
        (Just (Leaf (Symbol defSymbol)))
        (Just (Leaf defexpression)))
        = env { defines = defines env ++ [Define defSymbol defexpression] }
registerDefine env _ = env
