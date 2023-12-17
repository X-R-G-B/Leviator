{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- Defines
-}

module Computing.Defines
    (
        getSymbolValue
    ) where

import Types
import Computing.Errors

getSymbolValue :: Env -> String -> (Env, Maybe Tree)
getSymbolValue (Env { defines = [], errors = _, functions = _ }) _ =
    (Env { defines = [], errors = [], functions = [] }, Nothing)
getSymbolValue (Env { defines = (Define smbl value):xs,
    errors = err, functions = fcts }) expr
        | smbl == expr =
            (Env { defines = xs, errors = err, functions = fcts }, Just value)
        | otherwise = getSymbolValue
            (Env { defines = xs, errors = err, functions = fcts }) expr
