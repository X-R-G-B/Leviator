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

-- if opcode added, dont miss to add the right size in ./WasmUtils.hs
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
    FuncDef Bool String Int32 [Type] Type [OpCode] [(Type, Int32)]

instance Show FuncDef where
    show (FuncDef True name indexName paramsType returnType bodyCode vars) =
        "export fn " ++ show name ++ "{" ++ show indexName ++ "}("
        ++ show paramsType ++ ") -> " ++ show returnType ++ " {\n"
        ++ show bodyCode ++ "\n}\n" ++ show vars
    show (FuncDef False name indexName paramsType returnType bodyCode vars) =
        "fn " ++ show name ++ "{" ++ show indexName ++ "}("
        ++ show paramsType ++ ") -> " ++ show returnType ++ " {\n"
        ++ show bodyCode ++ "\n}\n" ++ show vars

instance Eq FuncDef where
    (==) (FuncDef aa aaa a b c d e) (FuncDef aa' aaa' a' b' c' d' e') =
        aa == aa' && aaa == aaa' && a == a'
        && b == b' && c == c' && d == d' && e == e'
