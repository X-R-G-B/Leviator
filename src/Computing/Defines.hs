{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- Defines
-}

module Defines
    (
        registerDefine,
        getSymbolValue
    ) where

import Types

getSymbolValue :: Env -> String -> (Env, Maybe Tree)
getSymbolValue (Env { defines = [], errors = _ }) _ = 
    (Env { defines = [], errors = [] }, Nothing)
getSymbolValue (Env { defines = (Define smbl value):xs, errors = err }) expr
    | smbl == expr = (Env { defines = xs, errors = err }, Just value)
    | otherwise = getSymbolValue (Env { defines = xs, errors = err }) expr

-- Register a define in the Defines list
registerDefine :: Env -> Symbol -> Tree -> Env
registerDefine env symb value =
    Env (defines env ++ [Define symb value]) (errors env)
