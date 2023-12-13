{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- Main
-}

import Computing.ComputeAST
import Parsing.Parser
import Types

printErrors :: (Env) -> IO ()
printErrors (Env defines_ []) = printErrors (Env defines_ ["Unable to compute"])
printErrors (Env defines_ errors_) = mapM_ putStrLn errors_ >> handleInput (Env defines_ [])

checkComputing :: (Env, Maybe Result) -> IO ()
checkComputing (env, Nothing) = printErrors env
checkComputing (env, Just result) = putStrLn (show result) >> handleInput env

checkParsing :: Maybe (Tree, String) -> Env -> IO ()
checkParsing Nothing _ = return ()
checkParsing (Just (tree, _)) env = checkComputing (computeAST env tree)

checkInput :: String -> Env -> IO ()
checkInput ":q" _ = return ()
checkInput input env = checkParsing (runParser (parseTree) input) env

handleInput :: Env -> IO ()
handleInput env = getLine >>= (\x -> checkInput x env)

main :: IO ()
main = handleInput (Env [] [])
