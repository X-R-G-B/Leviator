{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- Functions
-}

module Functions
    ( getFunctionByName
    ) where

import Types

-- Find and execute user defined function
getFunctionByName :: Env -> String -> Maybe Function
getFunctionByName (Env { functions = [] }) _ = Nothing
getFunctionByName (Env { functions = (Function fnName fnParams body):xs, defines = defs, errors = errs }) expr
    | fnName == expr = Just (Function fnName fnParams body)
    | otherwise = getFunctionByName (Env { functions = xs, defines = defs, errors = errs }) expr
