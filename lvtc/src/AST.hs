{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- AST
-}

module AST
  ( Type
  , Value (..)
  , Var
  , Symbol
  , FuncCall
  , FuncPrototype
  , FuncDeclaration
  , Instruction (..)
  , VarDeclaration
  , VarAssignation
  , Condition
  , Export
) where

import Data.Int (Int32)

type Symbol = String

type Type = String

data Value =
    Var String
    | FuncValue FuncCall
    | Boolean Bool
    | Integer Int32
    | Character Char
    | StringView String
    | Void

instance Show Value where
    show (Var x) = "V< " ++ show x ++ " >"
    show (FuncValue x) = "F< " ++ show x ++ " >"
    show (Boolean x) = "B< " ++ show x ++ " >"
    show (Integer x) = "I< " ++ show x ++ " >"
    show (Character x) = "C< " ++ show x ++ " >"
    show (StringView x) = "SV< " ++ show x ++ " >"
    show Void = "Void"

instance Eq Value where
    (==) (Var x) (Var y) = x == y
    (==) (FuncValue x) (FuncValue y) = x == y
    (==) (Boolean x) (Boolean y) = x == y
    (==) (Integer x) (Integer y) = x == y
    (==) (Character x) (Character y) = x == y
    (==) (StringView x) (StringView y) = x == y
    (==) Void Void = True
    (==) _ _ = False

-- Function

type Var = (Symbol, Type)

type Export = Bool

type FuncPrototype = (Export, Symbol, [Var], Type)

type FuncDeclaration = (FuncPrototype, [Instruction])


-- condition

type Condition = (Value, [Instruction], [Instruction])

-- Instruction

type FuncCall = (Symbol, [Value])

type VarDeclaration = (Var, Value)

type VarAssignation = (Symbol, Value)

data Instruction =
    Function FuncCall
    | Return Value
    | Declaration VarDeclaration
    | Assignation VarAssignation
    | Cond Condition

instance Show Instruction where
    show (Function x) =
        "Function[< " ++ show x ++ " >]"
    show (Return x) =
        "Return[< " ++ show x ++ " >]"
    show (Declaration x) =
        "Declaration[< " ++ show x ++ " >]"
    show (Assignation x) =
        "Assignation[< " ++ show x ++ " >]"
    show (Cond x) =
        "Cond[< " ++ show x ++ " >]"

instance Eq Instruction where
    (==) (Function x) (Function y) = x == y
    (==) (Return x) (Return y) = x == y
    (==) (Declaration x) (Declaration y) = x == y
    (==) (Assignation x) (Assignation y) = x == y
    (==) (Cond x) (Cond y) = x == y
    (==) _ _ = False
