{-
--  EPITECH PROJECT, 2023
--  Abstract Syntax Tree
--  File description:
--  ast
-}

module AST
    (
        Symbol,
        Atom (Number, Symbol, Boolean),
        Tree (Node, Leaf),
        showMaybeTree
    ) where

import Data.Int (Int64)

type Symbol = String

data Atom = Number Int64 | Symbol Symbol | Boolean Bool

data Tree = Node Symbol (Maybe Tree) (Maybe Tree) | Leaf Atom

showMaybeTree :: Maybe Tree -> String
showMaybeTree Nothing = "Nothing"
showMaybeTree (Just tree) = show tree

instance Show Atom where
  show (Number value) = "Number : " ++ show value
  show (Symbol value) = "Symbol : " ++ value
  show (Boolean value) = "Boolean : " ++ show value

instance Show Tree where
  show (Node value left right) = "Node : \"" ++ value ++ "\" left : \"" ++ showMaybeTree left ++ "\" right : \"" ++ showMaybeTree right ++ "\""
  show (Leaf value) = "Leaf : " ++ show value
