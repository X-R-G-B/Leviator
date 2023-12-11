{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- Compute nested lists
-}

module ComputeDeepLists
    (
        handleDeepList,
        resolveNestedLists
    ) where

import Types
import ComputeLists

-- Find nested lists and resolve them
resolveNestedLists :: Env -> [Tree] -> [Tree] -> (Env, Maybe [Tree])
resolveNestedLists env resolvedList [] = (env, Just resolvedList)
resolveNestedLists env resolvedList (List list : rest)
    | not (doesListContainsList list) =
        case handleSimpleList env list of
            (newEnv, Nothing) -> (newEnv, Nothing)
            (newEnv, Just resolved) ->
                resolveNestedLists newEnv (resolvedList ++ [resolved]) rest
    | otherwise =
        case resolveNestedLists env [] list of
            (newEnv, Nothing) -> (newEnv, Nothing)
            (newEnv, Just rvd)
                -> resolveNestedLists newEnv (resolvedList ++ [List rvd]) rest
resolveNestedLists env resolvedList (Number number : rest) =
    resolveNestedLists env (resolvedList ++ [Number number]) rest
resolveNestedLists env resolvedList (Boolean value : rest) =
    resolveNestedLists env (resolvedList ++ [Boolean value]) rest
resolveNestedLists env resolvedList (Symbol smbl : rest) =
    resolveNestedLists env (resolvedList ++ [Symbol smbl]) rest

-- Compute nested lists
handleDeepList :: Env -> [Tree] -> (Env, Maybe Result)
handleDeepList env list
    | not (doesListContainsList list) = handleSimpleList env list
    | otherwise =
        case resolveNestedLists env [] list of
            (newEnv, Nothing) -> (newEnv, Nothing)
            (newEnv, Just resolvedList) -> handleDeepList newEnv resolvedList
