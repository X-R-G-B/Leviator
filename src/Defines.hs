{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- Defines
-}

module Defines
    (
        registerDefine,
        registerFunction,
        getSymbolValue
    ) where

import Types
import Errors

getSymbolValue :: Env -> String -> (Env, Maybe Tree)
getSymbolValue (Env { defines = [], errors = _, functions = _ }) _ =
    (Env { defines = [], errors = [], functions = [] }, Nothing)
getSymbolValue (Env { defines = (Define smbl value):xs, errors = err }) expr
    | smbl == expr = (Env { defines = xs, errors = err, functions = [] }, Just value)
    | otherwise = getSymbolValue (Env { defines = xs, errors = err, functions = [] }) expr

-- Register a define in the Defines list
registerDefine :: Env -> Symbol -> Tree -> Env
registerDefine env symb value =
    Env (defines env ++ [Define symb value]) (errors env) (functions env)

-- Add a function to the Functions list in the Env
addFunction :: Env -> String -> [String] -> [Tree] -> Env
addFunction env name params bodies = Env (defines env) (errors env) (functions env ++ [Function name params bodies])

-- Get params from a function
getParams :: Tree -> [String]
getParams (List []) = []
getParams (List (Symbol smbl : xs)) = smbl : getParams (List xs)
getParams _ = []

-- Register a function in the Functions list
registerFunction :: Env -> Symbol -> Tree -> [Tree] -> Env
registerFunction env "" _ _ = registerError env "function name must not be empty"
registerFunction env name params bodies = addFunction env name (getParams params) bodies
