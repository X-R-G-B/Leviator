{-
-- EPITECH PROJECT, 2023
-- Abstract Syntax Tree
-- File description:
-- ast
-}

module AST
    (
        Symbol,
        Atom (Number, Symbol),
        Tree (Node, Leaf),
    ) where

import Data.Int (Int64)

type Symbol = String

data Atom = Number Int64 | Symbol Symbol

data Tree = Node Symbol [Tree] | Leaf Atom

instance Eq Atom where
    Number a == Number b = a == b
    Symbol a == Symbol b = a == b
    _ == _ = False

instance Show Atom where
    show (Number a) = "Number:'" ++ show a ++ "'"
    show (Symbol a) = "Symbol:'" ++ a ++ "'"

instance Eq Tree where
    Node a as == Node b bs = a == b && as == bs
    Leaf a == Leaf b = a == b
    _ == _ = False

instance Show Tree where
    show (Node a as) = "Node:'" ++ a ++ "'{" ++ show as ++ "}"
    show (Leaf a) = "Leaf:'" ++ show a ++ "'"
