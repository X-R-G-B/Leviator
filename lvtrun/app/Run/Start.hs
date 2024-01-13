{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Code
-}

module Run.Start
  (
    start
  )
where

import Data.Int (Int32, Int64)
import Control.Exception (throw)

import Types
import Errors
import Run.Vm
import Run.Functions

import Debug.Trace

start :: WasmModule -> IO ()
start wasmMod = do
  print wasmMod
  let res = startExecution (createVm wasmMod) (getStartFunctionId (exports wasmMod))
  return ()
