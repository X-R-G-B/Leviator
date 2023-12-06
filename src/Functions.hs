{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- ComputeAST
-}

module Functions
    (
        additionTree,
        substactionTree,
        multiplicationTree,
        divisionTree,
        moduloTree
    ) where

import AST
import Defines
import Data.Int (Int64)

-- Compute a "+ - div * mod" node, using defines if needed
-- Todo: See for an error handling and division by 0

additionTree :: Env -> Tree -> Int64
additionTree _ (Node "+" (Just (Leaf (Number left)))
    (Just (Leaf (Number right)))) = left + right
additionTree env (Node "+" (Just (Leaf (Number left)))
    (Just (Leaf (Symbol right)))) = left + getSymbolValue env right
additionTree env (Node "+" (Just (Leaf (Symbol left)))
    (Just (Leaf (Number right)))) = getSymbolValue env left + right
additionTree env (Node "+" (Just (Leaf (Symbol left)))
    (Just (Leaf (Symbol right)))) =
        getSymbolValue env left + getSymbolValue env right
additionTree _ _ = 0

substactionTree :: Env -> Tree -> Int64
substactionTree _ (Node "-" (Just (Leaf (Number left)))
    (Just (Leaf (Number right)))) = left - right
substactionTree env (Node "-" (Just (Leaf (Number left)))
    (Just (Leaf (Symbol right)))) = left - getSymbolValue env right
substactionTree env (Node "-" (Just (Leaf (Symbol left)))
    (Just (Leaf (Number right)))) = getSymbolValue env left - right
substactionTree env (Node "-" (Just (Leaf (Symbol left)))
    (Just (Leaf (Symbol right)))) =
        getSymbolValue env left - getSymbolValue env right
substactionTree _ _ = 0

multiplicationTree :: Env -> Tree -> Int64
multiplicationTree _ (Node "*" (Just (Leaf (Number left)))
    (Just (Leaf (Number right)))) = left * right
multiplicationTree env (Node "*" (Just (Leaf (Number left)))
    (Just (Leaf (Symbol right)))) = left * getSymbolValue env right
multiplicationTree env (Node "*" (Just (Leaf (Symbol left)))
    (Just (Leaf (Number right)))) = getSymbolValue env left * right
multiplicationTree env (Node "*" (Just (Leaf (Symbol left)))
    (Just (Leaf (Symbol right)))) =
        getSymbolValue env left * getSymbolValue env right
multiplicationTree _ _ = 0

divisionTree :: Env -> Tree -> Int64
divisionTree _ (Node "div" (Just (Leaf (Number left)))
    (Just (Leaf (Number right)))) = left `div` right
divisionTree env (Node "div" (Just (Leaf (Number left)))
    (Just (Leaf (Symbol right)))) = left `div` getSymbolValue env right
divisionTree env (Node "div" (Just (Leaf (Symbol left)))
    (Just (Leaf (Number right)))) = getSymbolValue env left `div` right
divisionTree env (Node "div" (Just (Leaf (Symbol left)))
    (Just (Leaf (Symbol right)))) =
        getSymbolValue env left `div` getSymbolValue env right
divisionTree _ _ = 0

moduloTree :: Env -> Tree -> Int64
moduloTree _ (Node "mod" (Just (Leaf (Number left)))
    (Just (Leaf (Number right)))) = left `mod` right
moduloTree env (Node "mod" (Just (Leaf (Number left)))
    (Just (Leaf (Symbol right)))) = left `mod` getSymbolValue env right
moduloTree env (Node "mod" (Just (Leaf (Symbol left)))
    (Just (Leaf (Number right)))) = getSymbolValue env left `mod` right
moduloTree env (Node "mod" (Just (Leaf (Symbol left)))
    (Just (Leaf (Symbol right)))) =
        getSymbolValue env left `mod` getSymbolValue env right
moduloTree _ _ = 0
