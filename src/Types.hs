{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- Defines all types avoiding circular dependencies
-}

module Types
    (
        Symbol,
        Tree(..),
        Define(..),
        Env(..),
        Result
    ) where

import Data.Int (Int64)

type Symbol = String

data Tree = Number Int64 | Symbol Symbol | Boolean Bool | List [Tree] | Empty

data Define = Define {
    symbol :: String,
    expression :: Tree
} deriving (Show)

data Env = Env {
    defines :: [Define],
    errors :: [String]
}

type Result = Tree

---------- EQ INSTANCES ----------

instance Eq Tree where
    Number a == Number b = a == b
    Symbol a == Symbol b = a == b
    Boolean a == Boolean b = a == b
    List a == List b = a == b
    Empty == Empty = True
    _ == _ = False

instance Eq Env where
    Env { defines = def1, errors = err1 } 
        == Env { defines = def2, errors = err2 }
            = def1 == def2 && err1 == err2

instance Eq Define where
    Define smb expr == Define smb2 expr2
        = smb == smb2 && expr == expr2

---------- SHOW INSTANCES ----------

instance Show Tree where
    show (Number a) = "N:'" ++ show a ++ "'"
    show (Symbol a) = "S:'" ++ a ++ "'"
    show (Boolean value) = "B: " ++ show value
    show (List list) = "L: " ++ show list
    show Empty = "Empty"

instance Show Env where
    show (Env { defines = def, errors = err }) =
        "Defines: " ++ show def ++ "\nErrors: " ++ show err
