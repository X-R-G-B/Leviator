{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- Compute simple lists
-}

module ComputeLists
    (
        doesListContainsList,
        handleSimpleList
    ) where

import Types
import Functions

doesListContainsList :: [Tree] -> Bool
doesListContainsList [] = False
doesListContainsList (List _ : _) = True
doesListContainsList (_ : rest) = doesListContainsList rest

-- Compute simple lists (no nested lists)
handleSimpleList :: Env -> [Tree] -> (Env, Maybe Result)
handleSimpleList env (Symbol "+" : rest) = addition env rest
handleSimpleList env (Symbol "*" : rest) = multiplication env rest
handleSimpleList env (Symbol "-" : rest) = subtraction env rest
handleSimpleList env (Symbol "/" : rest) = division env rest
handleSimpleList env (Symbol "%" : rest) = modulo env rest
handleSimpleList env _ = (env, Nothing)
