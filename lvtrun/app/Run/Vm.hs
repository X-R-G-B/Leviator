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
    addLabelIdx
  )
where

import Data.Int (Int32, Int64)
import Control.Exception (throw)

import Types
import Errors
import Run.Stack
import Run.Functions

data VM = VM {
  vmStack :: Stack,
  instructionIdx :: Int32,
  currentFunctionIdx :: Int32,
  labels :: [Int32],
  vmLocals :: [StackValue],
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

initFuncVars :: Function -> [StackValue]
initFuncVars (Function _ _ [] _) = []
initFuncVars (Function _ _ ((Local _ I32):xs) _) = (I_32 0):(initFuncVars (Function 0 0 xs []))
initFuncVars (Function _ _ ((Local _ I64):xs) _) = (I_64 0):(initFuncVars (Function 0 0 xs []))
initFuncVars (Function _ _ ((Local _ F32):xs) _) = (F_32 0.0):(initFuncVars (Function 0 0 xs []))
initFuncVars (Function _ _ ((Local _ F64):xs) _) = (F_64 0.0):(initFuncVars (Function 0 0 xs []))
initFuncVars _ = throw $ WasmError "initFuncVars: bad local"

initLocalVars :: Function -> FuncType -> [StackValue]
initLocalVars func funcType = (initFuncVars func)

---------------------------

initLocals :: VM -> VM
initLocals vm = do
  let fnTypes = types (wasmModule vm)
  let currentFunc = getFunctionFromId (currentFunctionIdx vm) (functions (wasmModule vm))
  let fnType = getFuncTypeFromId (funcType currentFunc) fnTypes
  let localVars = initLocalVars currentFunc fnType
  vm { vmLocals = localVars }
