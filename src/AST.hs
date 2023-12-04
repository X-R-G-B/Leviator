{-
-- EPITECH PROJECT, 2023
-- Abstract Syntax Tree
-- File description:
-- ast
-}

module AST
    (
        Symbol,
        Atom (Number, Symbol, Boolean),
        Tree (Node, Leaf, Variadic),
        showMaybeTree
    ) where

import Data.Int (Int64)

type Symbol = String

data Atom = Number Int64 | Symbol Symbol | Boolean Bool

data Tree = Node Symbol (Maybe Tree) (Maybe Tree) | Leaf Atom | Variadic (Maybe Tree) (Maybe Tree)

showMaybeTree :: Maybe Tree -> String
showMaybeTree Nothing = "Nothing"
showMaybeTree (Just tree) = show tree

instance Eq Atom where
    Number a == Number b = a == b
    Symbol a == Symbol b = a == b
    Boolean a == Boolean b = a == b
    _ == _ = False

instance Show Atom where
    show (Number a) = "N:'" ++ show a ++ "'"
    show (Symbol a) = "S:'" ++ a ++ "'"
    show (Boolean value) = "B: " ++ show value

instance Eq Tree where
    Node a fst_ scd == Node b bfst bscd = a == b && fst_ == bfst && scd == bscd
    Leaf a == Leaf b = a == b
    Variadic fst_ scd == Variadic bfst bscd = fst_ == bfst && scd == bscd
    _ == _ = False

instance Show Tree where
    show (Node value fst_ scd) = "Node:'" ++ value ++ "' first: '{" ++ showMaybeTree fst_ ++ "} second: {" ++ showMaybeTree scd ++ "}'"
    show (Leaf value) = "Leaf:'" ++ show value ++ "'"
    show (Variadic fst_ scd) = "Variadic first: {" ++ showMaybeTree fst_ ++ "} second: {" ++ showMaybeTree scd ++ "}"
