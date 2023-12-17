{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- Defines
-}

module Computing.Defines
    (
        getSymbolValue,
        addDefineToEnv
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
