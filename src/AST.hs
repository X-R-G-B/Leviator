{-
--  EPITECH PROJECT, 2023
--  Abstract Syntax Tree
--  File description:
--  ast
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
