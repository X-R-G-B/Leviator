{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- Builtins
-}

module Builtins
(
    getBuiltinsFunc
    , isBuiltinsFunc
    , getBuiltinsWat
    , getBuiltinWat
) where

import Text.Read (readMaybe)

import AST
import WatAST

getBuiltinsFuncOperator :: [FuncDeclaration]
getBuiltinsFuncOperator =
    [
        (("+", [("x", "Int"), ("y", "Int")], "Int"), []),
        (("-", [("x", "Int"), ("y", "Int")], "Int"), []),
        (("*", [("x", "Int"), ("y", "Int")], "Int"), []),
        (("/", [("x", "Int"), ("y", "Int")], "Int"), [])
    ]

getBuiltinsFuncComp :: [FuncDeclaration]
getBuiltinsFuncComp =
    [
        (("==", [("x", "Int"), ("y", "Int")], "Int"), []),
        (("<", [("x", "Int"), ("y", "Int")], "Int"), []),
        ((">", [("x", "Int"), ("y", "Int")], "Int"), []),
        (("<=", [("x", "Int"), ("y", "Int")], "Int"), []),
        ((">=", [("x", "Int"), ("y", "Int")], "Int"), []),
        (("!=", [("x", "Int"), ("y", "Int")], "Int"), [])
    ]

getBuiltinsFunc :: [FuncDeclaration]
getBuiltinsFunc = getBuiltinsFuncOperator ++ getBuiltinsFuncComp

getStackRet :: [OpCode] -> [OpCode]
getStackRet op = [LocalGet 0, LocalGet 1] ++ op ++ [WatAST.Return]

getBuiltinsWatOperator :: [FuncDef]
getBuiltinsWatOperator =
    [
        FuncDef 0 [I32, I32] I32 (getStackRet [I32Add]) [],
        FuncDef 1 [I32, I32] I32 (getStackRet [I32Sub]) [],
        FuncDef 2 [I32, I32] I32 (getStackRet [I32Mul]) [],
        FuncDef 3 [I32, I32] I32 (getStackRet [I32Div]) []
    ]

getBuiltinsWatComp :: [FuncDef]
getBuiltinsWatComp = [
        FuncDef 4 [I32, I32] I32 (getStackRet [I32EQ]) [],
        FuncDef 5 [I32, I32] I32 (getStackRet [I32LT_S]) [],
        FuncDef 6 [I32, I32] I32 (getStackRet [I32GT_S]) [],
        FuncDef 7 [I32, I32] I32 (getStackRet [I32LE_S]) [],
        FuncDef 8 [I32, I32] I32 (getStackRet [I32GE_S]) [],
        FuncDef 9 [I32, I32] I32 (getStackRet [I32NE]) []
    ]

getBuiltinsWat :: [FuncDef]
getBuiltinsWat = getBuiltinsWatOperator ++ getBuiltinsWatComp

isBuiltinsFuncString :: String -> [FuncDeclaration] -> Bool
isBuiltinsFuncString _ [] = False
isBuiltinsFuncString name (((name', _, _), _):xs)
    | name == name' = True
    | otherwise = isBuiltinsFuncString name xs

isBuiltinsFunc :: String -> Bool
isBuiltinsFunc indexName = case readMaybe indexName :: Maybe Int of
    Nothing -> isBuiltinsFuncString indexName getBuiltinsFunc
    Just x -> x < (length getBuiltinsFunc)

getBuiltinWat :: String -> FuncDef
getBuiltinWat indexName = case readMaybe indexName :: Maybe Int of
    Nothing -> error "builtins not found"
    Just x -> getBuiltinsWat !! x
