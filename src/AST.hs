{-
--  EPITECH PROJECT, 2023
--  Abstract Syntax Tree
--  File description:
--  Ast
-}

module AST
    (
        Symbol,
        Atom (Number, Symbol),
        Tree (Node, Leaf)
    ) where

import Data.Int (Int64)

type Symbol = String

data Atom = Number Int64 | Symbol Symbol

data Tree = Node Symbol (Maybe Tree) (Maybe Tree) | Leaf Atom

instance Show Atom where
    show (Number n) = show n
    show (Symbol s) = s
