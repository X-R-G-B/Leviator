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

-- TODO: Handle case where the define is a string / not defined
getSymbolValue :: Env -> String -> Int64
getSymbolValue (Env []) _ = 0
getSymbolValue (Env ((Define symbol value):rest)) symbolToFind
    | symbol == symbolToFind = case value of
        (Number number) -> number
        (Symbol _) -> 0
    | otherwise = getSymbolValue (Env rest) symbolToFind

------------ ADD SUB MUL FUNCTIONS ------------

-- Compute a "+" node, using defines if needed
-- Todo: See for better error handling in last line
additionTree :: Env -> Tree -> Int64
additionTree _ (Node "+" (Just (Leaf (Number left))) (Just (Leaf (Number right)))) = left + right
additionTree env (Node "+" (Just (Leaf (Number left))) (Just (Leaf (Symbol right)))) = left + getSymbolValue env right
additionTree env (Node "+" (Just (Leaf (Symbol left))) (Just (Leaf (Number right)))) = getSymbolValue env left + right
additionTree env (Node "+" (Just (Leaf (Symbol left))) (Just (Leaf (Symbol right)))) = getSymbolValue env left + getSymbolValue env right
additionTree _ _ = 0

-- Compute a "-" node, using defines if needed
-- Todo: See for better error handling in last line
substactionTree :: Env -> Tree -> Int64
substactionTree _ (Node "-" (Just (Leaf (Number left))) (Just (Leaf (Number right)))) = left - right
substactionTree env (Node "-" (Just (Leaf (Number left))) (Just (Leaf (Symbol right)))) = left - getSymbolValue env right
substactionTree env (Node "-" (Just (Leaf (Symbol left))) (Just (Leaf (Number right)))) = getSymbolValue env left - right
substactionTree env (Node "-" (Just (Leaf (Symbol left))) (Just (Leaf (Symbol right)))) = getSymbolValue env left - getSymbolValue env right
substactionTree _ _ = 0

-- Compute a "*" node, using defines if needed
-- Todo: See for better error handling in last line
multiplicationTree :: Env -> Tree -> Int64
multiplicationTree _ (Node "*" (Just (Leaf (Number left))) (Just (Leaf (Number right)))) = left * right
multiplicationTree env (Node "*" (Just (Leaf (Number left))) (Just (Leaf (Symbol right)))) = left * getSymbolValue env right
multiplicationTree env (Node "*" (Just (Leaf (Symbol left))) (Just (Leaf (Number right)))) = getSymbolValue env left * right
multiplicationTree env (Node "*" (Just (Leaf (Symbol left))) (Just (Leaf (Symbol right)))) = getSymbolValue env left * getSymbolValue env right
multiplicationTree _ _ = 0

-- Compute a "/" node, using defines if needed
-- Todo: See for better error handling in last line + division by 0
divisionTree :: Env -> Tree -> Int64
divisionTree _ (Node "/" (Just (Leaf (Number left))) (Just (Leaf (Number right)))) = left `div` right
divisionTree env (Node "/" (Just (Leaf (Number left))) (Just (Leaf (Symbol right)))) = left `div` getSymbolValue env right
divisionTree env (Node "/" (Just (Leaf (Symbol left))) (Just (Leaf (Number right)))) = getSymbolValue env left `div` right
divisionTree env (Node "/" (Just (Leaf (Symbol left))) (Just (Leaf (Symbol right)))) = getSymbolValue env left `div` getSymbolValue env right
divisionTree _ _ = 0

------------ ComputeNode ------------

computeNode :: Env -> Tree -> Int64
computeNode env tree@(Node "+" _ _) = additionTree env tree
computeNode env tree@(Node "-" _ _) = substactionTree env tree
computeNode env tree@(Node "*" _ _) = multiplicationTree env tree
computeNode env tree@(Node "/" _ _) = divisionTree env tree
-- TODO: Error handling
computeNode _ _ = 0

------------ Resolve deepest ------------

-- Compute the deepest node of the tree, it is when right and left node are a leaf and not a node
resolveDeepestNode :: Env -> Tree -> Tree
resolveDeepestNode env (Node symbol (Just (Leaf left)) (Just (Leaf right))) = Leaf (Number (computeNode env (Node symbol (Just (Leaf left)) (Just (Leaf right)))))
resolveDeepestNode env (Node symbol (Just (Leaf left)) (Just right)) = Node symbol (Just (Leaf left)) (Just $ resolveDeepestNode env right)
resolveDeepestNode env (Node symbol (Just left) (Just (Leaf right))) = Node symbol (Just $ resolveDeepestNode env left) (Just (Leaf right))
resolveDeepestNode env (Node symbol (Just left) (Just right)) = Node symbol (Just $ resolveDeepestNode env left) (Just $ resolveDeepestNode env right)
-- TODO: Error handling
resolveDeepestNode _ _ = (Leaf (Number 0))

------------ COMPUTE TREE ----------

computeTree :: Env -> Tree -> Atom
computeTree env (Leaf (Symbol symbol)) = Number (getSymbolValue env symbol)
computeTree _ (Leaf (Number number)) = Number number
computeTree env tree = computeTree env (resolveDeepestNode env tree)

------------ COMPUTE AST ------------

-- Call appropriate function depending on the node
computeAST :: Env -> Tree -> (Env, Maybe Atom)
computeAST env tree@(Node "define" _ _) = (registerDefine env tree, Nothing)
computeAST env tree = (env, Just (computeTree env tree))

-- Call computeAST on every tree in the list
computeAllAST :: Env -> [Tree] -> [Atom]
computeAllAST _ [] = []
computeAllAST env (tree:rest) = case atom of
    Just atom -> atom : computeAllAST newEnv rest
    Nothing -> computeAllAST newEnv rest
    where (newEnv, atom) = computeAST env tree
