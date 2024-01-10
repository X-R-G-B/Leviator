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

import Parsing.Parser
import Types
import IO

loadModule :: String -> IO WasmModule
loadModule path = getFileContent path >>= \bytes ->
  return $ parseModule bytes
