{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- Main
-}

import System.Environment (getArgs)
import Args
import Run
import Version

dispatchAction :: Either Args String -> IO ()
dispatchAction (Right error_) = putStrLn error_
dispatchAction (Left (Args ShowHelp _ _)) = printHelp
dispatchAction (Left (Args ShowVersion _ _)) = printVersion
dispatchAction (Left (Args Run RunFile f)) = runFile f
dispatchAction (Left (Args Run RunStdin _)) = runStdin

main :: IO ()
main = getArgs >>= (dispatchAction . parseArgs)
