{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- Abstract Syntax Tree
-}

module AST
    (
        Symbol,
        Tree (Number, Symbol, Boolean, List),
        showMaybeTree
    ) where

import Data.Int (Int64)

type Symbol = String

data Tree = Number Int64 | Symbol Symbol | Boolean Bool | List [Maybe Tree]

showMaybeTree :: Maybe Tree -> String
showMaybeTree Nothing = "Nothing"
showMaybeTree (Just tree) = show tree

instance Eq Tree where
    Number a == Number b = a == b
    Symbol a == Symbol b = a == b
    Boolean a == Boolean b = a == b
    List a == List b = a == b
    _ == _ = False

instance Show Tree where
    show (Number a) = "N:'" ++ show a ++ "'"
    show (Symbol a) = "S:'" ++ a ++ "'"
    show (Boolean value) = "B: " ++ show value
    show (List list) = "L: " ++ show list
