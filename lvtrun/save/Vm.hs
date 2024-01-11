{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Vm
-}

module Run.Vm
  (
    VM(..),
    incrementInstructionIdx,
    getInstructionIdxFromLabel,
    addLabelIdx,
    initLocals
  )
where

import Data.Int (Int32, Int64)
import Control.Exception (throw)

import Types
import Errors
import Run.Stack
import Run.Functions

import Debug.Trace

type VmLocals = [StackValue]

data VM = VM {
  vmStack :: Stack,
  instructionIdx :: Int32,
  currentFunctionIdx :: Int32,
  labels :: [Int32],
  vmLocals :: VmLocals,
  wasmModule :: WasmModule
} deriving (Show)

endInstruction :: VM -> VM
endInstruction (VM stack instructionIdx fnIdx lb lc wasmMod) = VM stack (instructionIdx + 1) fnIdx lb lc wasmMod

incrementInstructionIdx :: VM -> VM
incrementInstructionIdx (VM stack instructionIdx fnIdx lb lc wasmMod) = VM stack (instructionIdx + 1) fnIdx lb lc wasmMod

getInstructionIdxFromLabel :: VM -> Int32 -> Int32
getInstructionIdxFromLabel vm labelIdx
  | labelIdx < 0 = throw $ WasmError "getInstructionIdxFromLabel: bad label"
  | labelIdx >= fromIntegral (length (labels vm)) = throw $ WasmError "getInstructionIdxFromLabel: bad label"
  | otherwise = (labels vm) !! (fromIntegral labelIdx)

addLabelIdx :: VM -> Int32 -> VM
addLabelIdx vm labelIdx = vm { labels = labelIdx:(labels vm) }

---------------------------

setLocalWithParameters :: FuncType -> Stack -> VmLocals -> (VmLocals, Stack)
setLocalWithParameters _ stack [] = ([], stack)
setLocalWithParameters (FuncType id [] res) stack locals = trace ("here3") (locals, stack)
setLocalWithParameters (FuncType id (I32:xs) res) (Stack (I_32 x:xsStack)) (I_32 y:xsLocals)
  = trace ("here") setLocalWithParameters (FuncType id xs res) (Stack xsStack) (I_32 x:xsLocals)
setLocalWithParameters (FuncType id (I64:xs) res) (Stack (I_64 x:xsStack)) (I_64 y:xsLocals)
  = trace ("here2") setLocalWithParameters (FuncType id xs res) (Stack xsStack) (I_64 x:xsLocals)
setLocalWithParameters _ _ _ = throw $ WasmError "setLocalWithParameters: bad parameters"

initFuncVars :: Function -> VmLocals
initFuncVars (Function _ _ [] _) = []
initFuncVars (Function _ _ ((Local _ I32):xs) _) = (I_32 0):(initFuncVars (Function 0 0 xs []))
initFuncVars _ = throw $ WasmError "initFuncVars: bad local"

initLocalVars :: Function -> Stack -> FuncType -> (VmLocals, Stack)
initLocalVars func stack funcType = do
  let localsDefaultInit = initFuncVars func
  let nbOfLocals = length localsDefaultInit
  let values = stackPopN stack nbOfLocals


---------------------------

initLocals :: VM -> VM
initLocals vm = do
  let fnTypes = types (wasmModule vm)
  let currentFunc = getFunctionFromId (currentFunctionIdx vm) (functions (wasmModule vm))
  let fnType = getFuncTypeFromId (funcType currentFunc) fnTypes
  let (localVars, newStack) = initLocalVars currentFunc (vmStack vm) fnType
  vm { vmLocals = localVars, vmStack = newStack }


-- initFuncVars (Function _ _ ((Local _ I64):xs) _) = trace ("hh2") (I_64 0):(initFuncVars (Function 0 0 xs []))
-- initFuncVars (Function _ _ ((Local _ F32):xs) _) = trace ("hh3") (F_32 0.0):(initFuncVars (Function 0 0 xs []))
-- initFuncVars (Function _ _ ((Local _ F64):xs) _) = trace ("hh4") (F_64 0.0):(initFuncVars (Function 0 0 xs []))