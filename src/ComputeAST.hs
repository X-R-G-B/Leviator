{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- ComputeAST
-}

module ComputeAST
    (
        computeAST
    ) where

import Types
import Errors
import Defines
import Functions

-------------- HANDLE LISTS ------------

doesListContainsList :: [Tree] -> Bool
doesListContainsList [] = False
doesListContainsList (List list : _) = True
doesListContainsList (_ : rest) = doesListContainsList rest
doesListContainsList _ = False

-------------- HELPERS FOR NESTED LISTS ------------

-- Compute the most nested list
computeDeepest :: Env -> [Tree] -> (Env, Maybe Result)
computeDeepest env (Symbol "+" : rest) = addition env rest
computeDeepest env (Symbol "*" : rest) = multiplication env rest
computeDeepest env (Symbol "-" : rest) = subtraction env rest
computeDeepest env (Symbol "/" : rest) = division env rest
computeDeepest env (Symbol "%" : rest) = modulo env rest
computeDeepest env _ = (env, Nothing)

-- Find and resolve nested lists
resolveNestedLists :: Env -> [Tree] -> [Tree] -> (Env, Maybe [Tree])
resolveNestedLists env resolvedList [] = (env, Just resolvedList)
resolveNestedLists env resolvedList (List list : rest)
    | not (doesListContainsList list) =
        case computeDeepest env list of
            (newEnv, Nothing) -> (newEnv, Nothing)
            (newEnv, Just resolved) -> resolveNestedLists newEnv (resolvedList ++ [resolved]) rest
    | otherwise =
        case resolveNestedLists env [] list of
            (newEnv, Nothing) -> (newEnv, Nothing)
            (newEnv, Just resolved) -> resolveNestedLists newEnv (resolvedList ++ [List resolved]) rest
resolveNestedLists env resolvedList (Number number : rest) =
    resolveNestedLists env (resolvedList ++ [Number number]) rest
resolveNestedLists env resolvedList (Boolean value : rest) =
    resolveNestedLists env (resolvedList ++ [Boolean value]) rest
resolveNestedLists env resolvedList (Symbol symbol : rest) =
    resolveNestedLists env (resolvedList ++ [Symbol symbol]) rest

------------ COMPUTE SIMPLE AND NESTED LISTS ------------

-- Compute simple lists
handleSimpleList :: Env -> [Tree] -> (Env, Maybe Result)
handleSimpleList env (Symbol "+" : rest) = addition env rest
handleSimpleList env (Symbol "*" : rest) = multiplication env rest
handleSimpleList env (Symbol "-" : rest) = subtraction env rest
handleSimpleList env (Symbol "/" : rest) = division env rest
handleSimpleList env (Symbol "%" : rest) = modulo env rest
handleSimpleList env _ = (env, Nothing)

-- Compute nested lists
handleDeepList :: Env -> [Tree] -> (Env, Maybe Result)
handleDeepList env list
    | not (doesListContainsList list) = handleSimpleList env list
    | otherwise =
        case resolveNestedLists env [] list of
            (newEnv, Nothing) -> (newEnv, Nothing)
            (newEnv, Just resolvedList) -> handleDeepList newEnv resolvedList

-------------- COMPUTE NO LIST ------------

handleNoList :: Env -> Tree -> (Env, Maybe Result)
handleNoList env (Number number) = (env, Just (Number number))
handleNoList env (Boolean value) = (env, Just (Boolean value))
handleNoList env (Symbol symbol)
    | Nothing <- value = (env, Nothing)
    | Just (List list) <- value = computeAST env (List list)
    | Just result <- value = (env, Just result)
        where (_, value) = getSymbolValue env symbol

-------------- COMPUTE DEFINE ------------

handleDefine :: Env -> Tree -> (Env, Maybe Result)
handleDefine env (List [Symbol _, Symbol symbol, expression])
    = (registerDefine env symbol expression, Nothing)
handleDefine env _ = (registerError env "Bad define", Nothing)

-------------- COMPUTE AST ------------
computeAST :: Env -> Tree -> (Env, Maybe Result)
computeAST env tree@(List (Symbol "define" : _)) = handleDefine env tree
computeAST env tree@(List list)
    | doesListContainsList list = handleDeepList env list
    | otherwise = handleSimpleList env list
computeAST env tree = handleNoList env tree

-- List in the AST
