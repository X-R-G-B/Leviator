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
import Defines
import Errors
import ComputeDeepLists
import ComputeLists

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

-- Handle AST that register a define
handleDefine :: Env -> Tree -> (Env, Maybe Result)
handleDefine env (List [Symbol _, Symbol smbl, expr])
    = (registerDefine env smbl expr, Nothing)
handleDefine env _ = (registerError env "Bad define", Nothing)

-- Compute entire AST
computeAST :: Env -> Tree -> (Env, Maybe Result)
computeAST env tree@(List (Symbol "define" : _)) = handleDefine env tree
computeAST env (List list)
    | doesListContainsList list = handleDeepList env list
    | otherwise = handleSimpleList env list
computeAST env tree = handleNoList env tree
