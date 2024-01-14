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
) where

import Data.Int (Int32)

type Symbol = String

type Type = String

data Value =
  Var String | FuncValue FuncCall | Boolean Bool | Integer Int32 |
  StringView String


-- Function

type Var = (Symbol, Type)

type FuncPrototype = (Symbol, [Var], Type)

type FuncDeclaration = (FuncPrototype, [Instruction])


-- condition

type Condition = (Value, [Instruction], [Instruction])

-- Instruction

type FuncCall = (Symbol, [Value])

type VarDeclaration = (Var, Value)

type VarAssignation = (Symbol, Value)

data Instruction =
  Call FuncCall | Return Value | Declaration VarDeclaration |
  Assignation VarAssignation | Cond Condition
