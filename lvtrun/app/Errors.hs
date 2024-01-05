{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Errors
-}

module Errors
  (
    exitWithError
  )
where

import System.Exit (exitWith, ExitCode(..))

exitWithError :: String -> IO a
exitWithError msg = do
  putStrLn msg
  exitWith $ ExitFailure 84
