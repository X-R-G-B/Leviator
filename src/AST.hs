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
        Result (Integer, String, Bool, Array),
        showMaybeTree
    ) where

import Data.Int (Int64)

type Symbol = String

data Tree = Number Int64 | Symbol Symbol | Boolean Bool | List [Tree]

data Result = Integer Int64 | String String | Bool Bool | Array [Result]

showMaybeTree :: Maybe Tree -> String
showMaybeTree Nothing = "Nothing"
showMaybeTree (Just tree) = show tree

instance Eq Tree where
    Number a == Number b = a == b
    Symbol a == Symbol b = a == b
    Boolean a == Boolean b = a == b
    List a == List b = a == b
    _ == _ = False

instance Eq Result where
    Integer a == Integer b = a == b
    String a == String b = a == b
    Bool a == Bool b = a == b
    Array a == Array b = a == b
    _ == _ = False

instance Show Tree where
    show (Number a) = "N:'" ++ show a ++ "'"
    show (Symbol a) = "S:'" ++ a ++ "'"
    show (Boolean value) = "B: " ++ show value
    show (List list) = "L: " ++ show list

instance Show Result where
    show (Integer a) = "I:'" ++ show a ++ "'"
    show (String a) = "S:'" ++ a ++ "'"
    show (Bool value) = "B: " ++ show value
    show (Array list) = "L: " ++ show list
