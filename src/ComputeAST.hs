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

------------ ComputeNode ------------

--computeNode :: Env -> Tree -> Int64
--computeNode env tree@(Node "+" _ _) = additionTree env tree
--computeNode env tree@(Node "-" _ _) = substactionTree env tree
--computeNode env tree@(Node "*" _ _) = multiplicationTree env tree
--computeNode env tree@(Node "div" _ _) = divisionTree env tree
--computeNode env tree@(Node "mod" _ _) = moduloTree env tree
---- TODO: Error handling
--computeNode _ _ = 0
--
-------------- Resolve deepest ------------
--
--resolveDeepestNode :: Env -> Tree -> Tree
---- Node [Leaf] [Leaf]
--resolveDeepestNode env (Node symbol (Just (Leaf left)) (Just (Leaf right))) =
--    Leaf (Number (computeNode env
--        (Node symbol (Just (Leaf left)) (Just (Leaf right)))))
---- Node [Leaf] [Node]
--resolveDeepestNode env (Node symbol (Just (Leaf left)) (Just right)) =
--    Node symbol (Just (Leaf left)) (Just $ resolveDeepestNode env right)
---- Node [Node] [Leaf]
--resolveDeepestNode env (Node symbol (Just left) (Just (Leaf right))) =
--    Node symbol (Just $ resolveDeepestNode env left) (Just (Leaf right))
---- Node [Node] [Node]
--resolveDeepestNode env (Node symbol (Just left) (Just right)) =
--    Node symbol (Just $ resolveDeepestNode env left)
--        (Just $ resolveDeepestNode env right)
---- TODO: Error handling
--resolveDeepestNode _ _ = (Leaf (Number 0))


-------------- COMPUTE TREE ----------

--computeTree :: Env -> Tree -> Atom
--computeTree env (Leaf (Symbol symbol)) = Number (getSymbolValue env symbol)
--computeTree _ (Leaf (Number number)) = Number number
--computeTree _ (Leaf (Boolean value)) = Boolean value
--computeTree env tree = computeTree env (resolveDeepestNode env tree)

--computeTree :: Env -> Tree -> Atom
--computeTree env (Leaf (Symbol symbol)) = Number (getSymbolValue env symbol)
--computeTree _ (Leaf (Number number)) = Number number
--computeTree _ (Leaf (Boolean value)) = Boolean value
--computeTree env tree = computeTree env (resolveDeepestNode env tree)

doesListContainsList :: [Tree] -> Bool
doesListContainsList _ = False

-------------- HANDLE LISTS ------------

handleSimpleList :: Env -> [Tree] -> (Env, Maybe Result)
handleSimpleList env (Symbol "+" : rest) = addition env rest

handleDeepList :: Env -> [Tree] -> (Env, Maybe Result)
handleDeepList env _ = (env, Nothing)

-------------- COMPUTE NO LIST ------------

handleNoList :: Env -> Tree -> (Env, Maybe Result)
handleNoList env (Number number) = (env, Just (Number number))
handleNoList env (Boolean value) = (env, Just (Boolean value))
handleNoList env (Symbol symbol)
    | Nothing <- value = (env, Nothing)
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
