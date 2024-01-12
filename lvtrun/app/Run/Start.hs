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

getTypeFunc1 :: FuncType
getTypeFunc1 = FuncType 0 [I32, I32] [I32]

getTypeFunc2 :: FuncType
getTypeFunc2 = FuncType 1 [] [I32]

getTestAddFunction :: Function
getTestAddFunction = Function 0 2 [Local 0 I32, Local 1 I32] [GetLocal 0, GetLocal 1, I32Add, End]

getMainTestFunction :: Function
getMainTestFunction = Function 1 1 [] [I32Const 10, I32Const 5, Call 2, End]

getFunctionsForWasmMod :: [Function]
getFunctionsForWasmMod = [getTestAddFunction, getMainTestFunction]

start :: WasmModule -> IO ()
start wasmMod = do
  let newWasmMod = wasmMod { functions = getFunctionsForWasmMod, types = [getTypeFunc1, getTypeFunc2] }
  startExecution (createVm newWasmMod) 1
