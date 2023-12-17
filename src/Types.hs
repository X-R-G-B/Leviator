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
        Result,
        Function(..)
    ) where

import Data.Int (Int64)
import Data.Void (Void)

type Symbol = String

data Tree = Number Int64 | Symbol Symbol | Boolean Bool | List [Tree]

data Define = Define {
    symbol :: String,
    expression :: Tree
} deriving (Show)

data Function = Function {
    name :: String,
    params :: [String],
    bodies :: [Tree]
} deriving (Show)

data Env = Env {
    defines :: [Define],
    errors :: [String],
    functions :: [Function]
}

type Result = Either (Maybe Tree) Void

---------- EQ INSTANCES ----------

instance Eq Tree where
    Number a == Number b = a == b
    Symbol a == Symbol b = a == b
    Boolean a == Boolean b = a == b
    List a == List b = a == b
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

instance Show Env where
    show (Env { defines = def, errors = err, functions = func }) =
        "Defines: " ++ show def ++ "\nErrors: " ++ show err ++ "\nFunctions: " ++ show func
