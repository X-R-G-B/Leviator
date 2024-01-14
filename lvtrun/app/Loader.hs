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

import Types (WasmModule)
import IO (getFileContent)
import Parsing.Parser (parseModule)
import Errors (CustomException(..))

getFilePath :: IO String
getFilePath = getArgs >>= \args ->
  case args of
    [path] -> return path
    _ -> throw $ UsageError "Usage: ./run <file.wasm>"

loadModule :: IO WasmModule
loadModule = getFilePath >>= \filePath ->
  getFileContent filePath >>= \bytes ->
    return $ parseModule bytes
