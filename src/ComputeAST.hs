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
import ListContainList
import Defines
import Errors
import ReplaceFunctionParams
import Functions

-- Find nested lists and resolve them
resolveNestedLists :: Env -> [Tree] -> [Tree] -> (Env, Maybe [Tree])
resolveNestedLists env resolvedList [] = (env, Just resolvedList)
resolveNestedLists env resolvedList (List list : rest)
    | not (doesListContainsList list) =
        case handleSimpleList env list of
            (newEnv, Nothing) -> (newEnv, Nothing)
            (newEnv, Just resolved) ->
                resolveNestedLists newEnv (resolvedList ++ [resolved]) rest
    | otherwise = case resolveNestedLists env [] list of
            (newEnv, Nothing) -> (newEnv, Nothing)
            (newEnv, Just rvd)
                -> resolveNestedLists newEnv (resolvedList ++ [List rvd]) rest
resolveNestedLists env resolvedList (Number number : rest) =
    resolveNestedLists env (resolvedList ++ [Number number]) rest
resolveNestedLists env resolvedList (Boolean value : rest) =
    resolveNestedLists env (resolvedList ++ [Boolean value]) rest
resolveNestedLists env resolvedList (Symbol smbl : rest) =
    resolveNestedLists env (resolvedList ++ [Symbol smbl]) rest

    -- Compute simple lists (no nested lists)
handleSimpleList :: Env -> [Tree] -> (Env, Maybe Result)
handleSimpleList env (Symbol "+" : rest) = addition env rest
handleSimpleList env (Symbol "*" : rest) = multiplication env rest
handleSimpleList env (Symbol "-" : rest) = subtraction env rest
handleSimpleList env (Symbol "div" : rest) = division env rest
handleSimpleList env (Symbol "mod" : rest) = modulo env rest
handleSimpleList env (Symbol smbl : rest) =
    case getFunctionByName env smbl of
        Just func ->
            let (newEnv, result) = computeFunction env func rest
            in case result of
                Just res -> (env, Just res)
                Nothing -> (registerError env ("Function " ++ smbl ++ " not found"), Nothing)
        Nothing   -> (registerError env ("Function " ++ smbl ++ " not found"), Nothing)
handleSimpleList env _ = (registerError env "Bad function call", Nothing)

-- Compute nested lists
handleDeepList :: Env -> [Tree] -> (Env, Maybe Result)
handleDeepList env list
    | not (doesListContainsList list) = handleSimpleList env list
    | otherwise =
        case resolveNestedLists env [] list of
            (newEnv, Nothing) -> (newEnv, Nothing)
            (newEnv, Just resolvedList) -> handleDeepList newEnv resolvedList

handleLambda :: Env -> Tree -> (Env, Maybe Result)
handleLambda env (List (List (Symbol "lambda" : List params : bodies): (List args): _))
    = computeFunction env (Function "" (getParams (List params)) bodies) args
handleLambda env _ = (registerError env "Bad lambda", Nothing)

computeFunction' :: Env -> Function -> [Tree] -> (Env, Maybe Result)
computeFunction' env (Function _ _ []) _ = (env, Nothing)
computeFunction' env (Function name params (x:_)) args =
    case replaceFunctionParams env params x args of
        (newEnv, Nothing) -> (newEnv, Nothing)
        (newEnv, Just replaced) -> computeAST newEnv replaced

computeFunction :: Env -> Function -> [Tree] -> (Env, Maybe Result)
computeFunction env (Function _ _ []) _ = (env, Nothing)
computeFunction env (Function name params (x:xs:rest)) args =
    case computeFunction' env (Function name params [x]) args of
        (newEnv, Nothing) -> computeFunction newEnv (Function name params (xs:rest)) args
        (newEnv, Just replaced) -> (registerError newEnv "Return needs to be the last statement", Nothing)
computeFunction env (Function name params (x:_)) args =
    case computeFunction' env (Function name params [x]) args of
        (newEnv, Nothing) -> (registerError newEnv "Missing return in function", Nothing)
        (newEnv, Just replaced) -> computeAST newEnv replaced
computeFunction env _ _ = (registerError env "Bad function call", Nothing)

-- Handle AST that doesn't contain a list
handleNoList :: Env -> Tree -> (Env, Maybe Result)
handleNoList env (Number nbr) = (env, Just (Number nbr))
handleNoList env (Boolean value) = (env, Just (Boolean value))
handleNoList env (Symbol smbl)
    | Nothing <- value = (env, Nothing)
    | Just (List list) <- value = computeAST env (List list)
    | Just result <- value = (env, Just result)
        where (_, value) = getSymbolValue env smbl
handleNoList env _ = (env, Nothing)

-- Compute entire AST
computeAST :: Env -> Tree -> (Env, Maybe Result)
computeAST env tree@(List (Symbol "define" : _)) = handleDefine env tree
--(List [List [Symbol "lambda", L
computeAST env tree@(List (List (Symbol "lambda" : _) : _)) = handleLambda env tree
computeAST env (List list)
    | doesListContainsList list = handleDeepList env list
    | otherwise = handleSimpleList env list
computeAST env tree = handleNoList env tree
