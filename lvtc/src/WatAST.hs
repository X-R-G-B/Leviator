{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- ShuntingYard
-}

module WatAST
(
    OpCode (..)
    , Type (..)
    , FuncDef (..)
) where

import Data.Int (Int32)

data OpCode =
    LocalGet Int32
    | LocalSet Int32
    | I32Const Int32
    | I32Store
    | I32Load
    | I32GT_S
    | I32GE_S
    | I32LT_S
    | I32LE_S
    | I32EQ
    | I32NE
    | I32Add
    | I32Sub
    | I32Mul
    | I32Div
    | Return
    | Call Int32
    | If
    | Else
    | End
    deriving (Show, Eq)

data Type =
    I32

instance Show Type where
    show I32 = "i32"

instance Eq Type where
    (==) I32 I32 = True

data FuncDef =
    FuncDef Int32 [Type] Type [OpCode] [(Type, Int32)]

instance Show FuncDef where
    show (FuncDef indexName paramsType returnType bodyCode vars) =
        show indexName ++ "(" ++ show paramsType ++ ") -> " ++ show returnType
        ++ " {\n" ++ show bodyCode ++ "\n}\n" ++ show vars

instance Eq FuncDef where
    (==) (FuncDef a b c d e) (FuncDef a' b' c' d' e') =
        a == a' && b == b' && c == c' && d == d' && e == e'
