{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Main
-}

module Main (main) where

import Control.Exception (try)

import Loader (loadModule)
import Errors (handleException)
import Run.Start (startExecution)

main :: IO ()
main = try (startExecution =<< loadModule) >>= \result ->
  case result of
    Left err -> handleException err
    Right _ -> return ()
