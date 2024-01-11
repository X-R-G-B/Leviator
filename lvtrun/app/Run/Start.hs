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
import Run.Types

import Debug.Trace

--------------------------------------

getStartFunctionId :: [Export] -> Int32
getStartFunctionId [] = throw $ WasmError "No start function"
getStartFunctionId (x:xs)
  | expName x == "_start" =
    case expDesc x of
      ExportFunc idx -> idx
      _ -> throw $ WasmError "getStartFunctionId: bad export"
  | otherwise = getStartFunctionId xs
getStartFunctionId _ = throw $ WasmError "getStartFunctionId: bad export"

getFunctionFromId :: Int32 -> [Function] -> Function
getFunctionFromId id [] = throw $ WasmError "getFunctionFromId: bad id"
getFunctionFromId id (x:xs)
  | funcIdx x == id = x
  | otherwise = getFunctionFromId id xs

getStartFunction :: [Export] -> [Function] -> Function
getStartFunction exports functions =
  getFunctionFromId (getStartFunctionId exports) functions

-----------------------------

executeInstruction :: VMConfig -> Instruction -> VMConfig
executeInstruction (VMConfig stack instructionIdx fnIdx wasmMod) (Block _) = VMConfig stack (instructionIdx + 1) fnIdx wasmMod
executeInstruction (VMConfig stack instructionIdx fnIdx wasmMod) (End) = VMConfig stack (instructionIdx + 1) fnIdx wasmMod
executeInstruction (VMConfig stack instructionIdx fnIdx wasmMod) (I32Const value) = VMConfig (addStackValue stack (I_32 value)) (instructionIdx + 1) fnIdx wasmMod
executeInstruction (VMConfig stack instructionIdx fnIdx wasmMod) (I32Eqz) = do
  case stackDrop stack of
    I_32 0 -> trace "true" (VMConfig (addStackValue stack (I_32 1)) (instructionIdx + 1) fnIdx wasmMod)
    _ -> trace "false" (VMConfig (addStackValue stack (I_32 0)) (instructionIdx + 1) fnIdx wasmMod)
executeInstruction (VMConfig stack instructionIdx fnIdx wasmMod) (BrIf labelIdx)
  | stackDrop stack == I_32 0 = executeInstruction (VMConfig stack (instructionIdx + 1) fnIdx wasmMod) (Br labelIdx)
  | otherwise = executeInstruction (VMConfig stack (instructionIdx + 1) fnIdx wasmMod) (Br labelIdx)
executeInstruction vmCOnf (Call funcIdx) = trace ("call") execFunction (vmCOnf { instructionIdx = 0 }) (getFunctionFromId funcIdx (functions (wasmModule vmCOnf)))
executeInstruction vmConf Unreachable = throw $ WasmError "Unreachable"
executeInstruction vmConf _ = vmConf

------------------------------

execFunction :: VMConfig -> Function -> VMConfig
execFunction config (Function typeIdx funcIdx locals body) = do
  let vmConf = trace ("execFunction: " ++ show body) config { currentFunctionIdx = funcIdx }
  let finalVMConf = foldl executeInstruction vmConf body
  finalVMConf

start :: WasmModule -> IO ()
start wasmMod = do
  let configVm = VMConfig (Stack []) 0 0 wasmMod
  let startFunc = getStartFunction (exports wasmMod) (functions wasmMod)
  let finalVMConf = execFunction configVm startFunc
  print (vmStack finalVMConf)
  return ()
