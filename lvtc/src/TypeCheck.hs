{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- Type checker
-}

module Lib
    ( someFunc
    ) where

import AST

functions :: [FuncDeclaration],

checkFuncCall

checkCondition :: Condition -> TypeEnv -> Maybe TypeEnv
checkCondition (Symbol s, xs, ys) env =
checkCondition (Type t, xs, ys) env =
checkCondition (FuncCall (symbol, values), xs, ys) env = checkBoolFunc (checkFuncCall (FuncCall (symbol, values)) env) xs ys
checkCondition _ _ = Nothing

checkInstructions :: [Instruction] -> TypeEnv -> Maybe TypeEnv
checkInstructions ((Symbol s):xs) env =
checkInstructions ((Type t):xs) env =
checkInstructions ((FuncCall f):xs) env =
checkInstructions ((Return r):xs) env =
checkInstructions ((VarDeclaration v):xs) env =
checkInstructions ((Condition c):xs) env = checkCondition c env
checkInstructions _ _ = Nothing

checkFunctionInstructions :: Maybe TypeEnv -> [ASTExpression] -> Maybe TypeEnv
checkFunctionInstructions (Just env) xs = typeCheck xs env
checkFunctionInstructions _ _ _ = Nothing

checkFunc :: FuncDeclaration -> TypeEnv -> [ASTExpression] -> Maybe TypeEnv
checkFunc (FuncDeclaration (Prototype (symbol, vars, (Void))) []) env xs = Just env
checkFunc (FuncDeclaration _ []) env xs = Nothing
checkFunc (FuncDeclaration prototype instructions) env xs = checkFuncInstructions (checkInstructions instructions env) xs
checkFunc _ _ _ = Nothing

typeCheck :: [FuncDeclaration] -> TypeEnv -> Maybe TypeEnv
typeCheck ((FuncDeclaration func):xs) (Env functions a) = checkFunc func (Env functions ++ [func] a) xs
typeCheck ((Alias symbol value):xs) env = checkAlias (Alias symbol value) env xs
typeCheck _ _ = Nothing

handleTypeCheck :: [FuncDeclaration] -> Maybe TypeEnv
handleTypeCheck expressions = typeCheck expressions (TypeEnv [] [])
