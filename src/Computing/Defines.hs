{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- Defines
-}

module Computing.Defines
    (
        getSymbolValue,
        addDefineToEnv,
        registerFunction,
        getParams
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

isAlreadyDefined :: Env -> Symbol -> Bool
isAlreadyDefined env symb = symb `elem` map (\(Define s _) -> s) (defines env)

addDefineToEnv :: Env -> Symbol -> Tree -> Env
addDefineToEnv env symb value
    | isAlreadyDefined env symb = registerError env ("Symbol " ++ symb ++
        " is already defined")
    | otherwise = Env (defines env ++ [Define symb value]) (errors env)
        (functions env)

-- Add a function to the Functions list in the Env
addFunction :: Env -> String -> [String] -> [Tree] -> Env
addFunction env fnName fnParams fnBodies
    = Env (defines env) (errors env)
        (functions env ++ [Function fnName fnParams fnBodies])

-- Get params from a function
getParams :: Tree -> [String]
getParams (List []) = []
getParams (List (Symbol smbl : xs)) = smbl : getParams (List xs)
getParams _ = []

-- Register a function in the Functions list
registerFunction :: Env -> Symbol -> Tree -> [Tree] -> Env
registerFunction env "" _ _ =
    registerError env "function name must not be empty"
registerFunction env fnName fnParams fnBodies
    = addFunction env fnName (getParams fnParams) fnBodies
