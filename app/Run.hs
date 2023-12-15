{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- run
-}

module Run
    (
        runStdin,
        runFile
    ) where

import Computing.ComputeAST
import Parsing.Parser
import Types
import System.IO

data HHHandle = HHHandle Handle Bool

printErrors :: HHHandle -> Env -> IO ()
printErrors hand (Env defines_ []) =
  printErrors hand (Env defines_ ["Unable to compute"])
printErrors hand (Env defines_ errors_) =
  mapM_ putStrLn errors_ >> handleInput hand (Env defines_ [])

checkComputing :: HHHandle -> (Env, Maybe Result) -> IO ()
checkComputing hand (env, Nothing) = printErrors hand env
checkComputing hand (env, Just result) = print result >> handleInput hand env

checkParsing :: HHHandle -> Maybe (Tree, String) -> Env -> IO ()
checkParsing _ Nothing _ = return ()
checkParsing hand (Just (tree, _)) env =
    checkComputing hand (computeAST env tree)

checkInput :: HHHandle -> String -> Env -> IO ()
checkInput _ ":q" _ = return ()
checkInput hand input env = checkParsing hand (runParser (parseTree) input) env

checkEOF :: HHHandle -> Env -> Bool -> IO ()
checkEOF _ _ True = return ()
checkEOF (HHHandle ff shouldClosee) env False = hGetLine ff >>=
    (\x -> checkInput (HHHandle ff shouldClosee) x env)

handleInput :: HHHandle -> Env -> IO ()
handleInput (HHHandle ff shouldClosee) env =
    hIsEOF ff >>= (\x -> checkEOF (HHHandle ff shouldClosee) env x)

runStdin :: IO ()
runStdin = runFileHandle stdin False

runFile :: String -> IO ()
runFile filePath = openFile filePath ReadMode >>= \x -> runFileHandle x True

onEnd :: HHHandle -> IO ()
onEnd (HHHandle ff True) = hClose ff
onEnd _ = return ()

runFileHandle :: Handle -> Bool -> IO ()
runFileHandle ff shouldClosee =
    handleInput (HHHandle ff shouldClosee) (Env [] []) >>
    onEnd (HHHandle ff shouldClosee)
