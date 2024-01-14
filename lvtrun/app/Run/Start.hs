{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Code
-}

module Run.Start
(
  startExecution
)
where

import Data.Int (Int32, Int64)
import Control.Exception (throw)

import Types
import Errors
import Run.Vm
import Run.Functions

startExecution :: WasmModule -> IO ()
startExecution wasmMod = startExecution2 (createVm wasmMod) (getStartFunctionId (exports wasmMod))
