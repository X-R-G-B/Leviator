{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- ComputeAST
-}

module ComputeAST
    (
        computeAST,
        computeAllAST
    ) where

import AST
import Defines
import Data.Int (Int64)
import Functions

------------ ComputeNode ------------

computeNode :: Env -> Tree -> Int64
computeNode env tree@(Node "+" _ _) = additionTree env tree
computeNode env tree@(Node "-" _ _) = substactionTree env tree
computeNode env tree@(Node "*" _ _) = multiplicationTree env tree
computeNode env tree@(Node "div" _ _) = divisionTree env tree
computeNode env tree@(Node "mod" _ _) = moduloTree env tree
-- TODO: Error handling
computeNode _ _ = 0

------------ Resolve deepest ------------

resolveDeepestNode :: Env -> Tree -> Tree
-- Node [Leaf] [Leaf]
resolveDeepestNode env (Node symbol (Just (Leaf left)) (Just (Leaf right))) =
    Leaf (Number (computeNode env
        (Node symbol (Just (Leaf left)) (Just (Leaf right)))))
-- Node [Leaf] [Node]
resolveDeepestNode env (Node symbol (Just (Leaf left)) (Just right)) =
    Node symbol (Just (Leaf left)) (Just $ resolveDeepestNode env right)
-- Node [Node] [Leaf]
resolveDeepestNode env (Node symbol (Just left) (Just (Leaf right))) =
    Node symbol (Just $ resolveDeepestNode env left) (Just (Leaf right))
-- Node [Node] [Node]
resolveDeepestNode env (Node symbol (Just left) (Just right)) =
    Node symbol (Just $ resolveDeepestNode env left)
        (Just $ resolveDeepestNode env right)
-- TODO: Error handling
resolveDeepestNode _ _ = (Leaf (Number 0))

------------ COMPUTE TREE ----------

computeTree :: Env -> Tree -> Atom
computeTree env (Leaf (Symbol symbol)) = Number (getSymbolValue env symbol)
computeTree _ (Leaf (Number number)) = Number number
computeTree _ (Leaf (Boolean value)) = Boolean value
computeTree env tree = computeTree env (resolveDeepestNode env tree)

------------ COMPUTE AST ------------

computeAST :: Env -> Tree -> (Env, Maybe Atom)
computeAST env tree@(Node "define" _ _) = (registerDefine env tree, Nothing)
computeAST env tree = (env, Just (computeTree env tree))

-- Call computeAST on every tree in the list
computeAllAST :: Env -> [Tree] -> [Atom]
computeAllAST _ [] = []
computeAllAST env (tree:rest) = case atom' of
    Just atom -> atom : computeAllAST newEnv rest
    Nothing -> computeAllAST newEnv rest
    where (newEnv, atom') = computeAST env tree
