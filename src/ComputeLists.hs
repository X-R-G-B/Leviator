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
import Errors

doesListContainsList :: [Tree] -> Bool
doesListContainsList [] = False
doesListContainsList (List _ : _) = True
doesListContainsList (_ : rest) = doesListContainsList rest

-- Compute simple lists (no nested lists)
handleSimpleList :: Env -> [Tree] -> (Env, Maybe Result)
handleSimpleList env (Symbol "+" : rest) = addition env rest
handleSimpleList env (Symbol "*" : rest) = multiplication env rest
handleSimpleList env (Symbol "-" : rest) = subtraction env rest
handleSimpleList env (Symbol "div" : rest) = division env rest
handleSimpleList env (Symbol "mod" : rest) = modulo env rest
handleSimpleList env (Symbol smbl : rest)
    | isAFunction env smbl = (env, Just (Number 4242))
    | otherwise = (registerError env ("Function " ++ smbl ++ " not found"), Nothing)
handleSimpleList env _ = (registerError env "Bad function call", Nothing)
