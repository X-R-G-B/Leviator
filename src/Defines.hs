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
        getSymbolValue,
        getParams,
        handleDefine
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
addFunction env fnName fnParams fnBodies
    = Env (defines env) (errors env) (functions env ++ [Function fnName fnParams fnBodies])

-- Get params from a function
getParams :: Tree -> [String]
getParams (List []) = []
getParams (List (Symbol smbl : xs)) = smbl : getParams (List xs)
getParams _ = []

-- Register a function in the Functions list
registerFunction :: Env -> Symbol -> Tree -> [Tree] -> Env
registerFunction env "" _ _ = registerError env "function name must not be empty"
registerFunction env fnName fnParams fnBodies
    = addFunction env fnName (getParams fnParams) fnBodies

handleDefine :: Env -> Tree -> (Env, Result)
handleDefine env (List [Symbol _, Symbol smbl, List (Symbol "lambda": List fnParams : fnBodies)])
    = (registerFunction env smbl (List fnParams) fnBodies, Left (Nothing))
handleDefine env (List [Symbol _, (List (Symbol smbl : fnParams)), List fnBodies])
    = (registerFunction env smbl (List fnParams) (List fnBodies : []), Left (Nothing))
handleDefine env (List [Symbol _, Symbol smbl, expr]) = (registerDefine env smbl expr, Left (Nothing))
handleDefine env _ = (registerError env "Bad define", Right (undefined))
