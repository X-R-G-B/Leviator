{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- ComputeAST
-}

module ComputeAST
    (
        computeASTs
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

------------ ComputeNode ------------

computeNode :: Env -> Tree -> Int64
computeNode env tree@(Node "+" _ _) = additionTree env tree
computeNode env tree@(Node "-" _ _) = substactionTree env tree
computeNode env tree@(Node "*" _ _) = multiplicationTree env tree
-- TODO: Error handling
computeNode _ _ = 0

------------ COMPUTE TREE ------------

-- Call appropriate function depending on the node
computeAST :: Env -> Tree -> IO Env
computeAST env tree@(Node "define" _ _) = return $ registerDefine env tree
computeAST env tree = print (computeNode env tree) >> return env

-- Call computeAST on every tree in the list
computeASTs :: Env -> [Tree] -> IO ()
computeASTs _ [] = return ()
computeASTs env (tree:rest) = do
    updatedEnv <- computeAST env tree
    computeASTs updatedEnv rest
