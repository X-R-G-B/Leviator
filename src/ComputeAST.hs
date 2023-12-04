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

-- Temporary function to compute defines only
computeAST :: Env -> Tree -> Env
computeAST env tree = registerDefine env tree

-- Call computeAST on every tree in the list
computeASTs :: [Tree] -> IO ()
computeASTs tree = print (foldl computeAST (Env []) tree) >> return ()
