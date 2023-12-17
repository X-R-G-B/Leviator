{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- Errors
-}

module Computing.Errors
    (
        registerError,
        getErrors,
        getLastError,
        clearErrors,
        printErrors,
        printLastError
    ) where

import Types

-- Add a new error to env
registerError :: Env -> String -> Env
registerError env err = Env (defines env) (errors env ++ [err]) (functions env)

-- Get all errors
getErrors :: Env -> [String]
getErrors env = errors env

-- Get the last error
getLastError :: Env -> String
getLastError env = last (errors env)

-- Clear all errors
clearErrors :: Env -> Env
clearErrors env = Env (defines env) [] (functions env)

-- Print all errors
printErrors :: Env -> IO ()
printErrors env = mapM_ putStrLn (map (\x -> "Error: " ++ x) (errors env))

-- Print the last error
printLastError :: Env -> IO ()
printLastError env = putStrLn (last (errors env))
