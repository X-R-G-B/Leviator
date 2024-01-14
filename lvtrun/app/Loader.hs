{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Loader
-}

module Loader
(
  loadModule
)
where

import System.Environment (getArgs)
import Control.Exception (throw)

import Parsing.Parser
import Types
import IO
import Errors

getFilePath :: IO String
getFilePath = do
  args <- getArgs
  case args of
    [path] -> return path
    _ -> throw $ UsageError "Usage: ./run <file.wasm>"

loadModule :: IO WasmModule
loadModule = do
  filePath <- getFilePath
  getFileContent filePath >>= \bytes ->
    return $ parseModule bytes
