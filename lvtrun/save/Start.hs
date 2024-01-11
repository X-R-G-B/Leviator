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
import Run.Stack
import Run.Functions

import Debug.Trace


-----------------------------

executeInstruction :: VM -> Instruction -> VM
executeInstruction vm (Block _) = addLabelIdx vm (instructionIdx vm)
executeInstruction vm (Nop) = vm
executeInstruction vm (End) = vm
executeInstruction vm (I32Const value) = vm { vmStack = addStackValue (vmStack vm) (I_32 value) }
executeInstruction vm (I32Eqz) = case stackDrop (vmStack vm) of
    I_32 0 -> vm { vmStack = addStackValue (vmStack vm) (I_32 1) }
    _ -> vm { vmStack = addStackValue (vmStack vm) (I_32 0) }
executeInstruction vm (Call idx) = execFunction vm (getFunctionFromId idx (functions (wasmModule vm)))
executeInstruction vm (BrIf lIdx) = case stackDrop (vmStack vm) of
    I_32 0 -> vm
    _ -> executeInstruction (vm { instructionIdx = getInstructionIdxFromLabel vm lIdx }) (Nop)
executeInstruction vm (I32Add) = do
  let (var1, vmStack') = stackPop (vmStack vm)
  let (var2, vmStack'') = stackPop vmStack'
  case (var1, var2) of
    (I_32 x, I_32 y) -> vm { vmStack = addStackValue vmStack'' (I_32 (x + y)) }
    _ -> throw $ WasmError "I32Add: bad parameters"
executeInstruction vm (GetLocal idx) =
  let local = (vmLocals vm) !! (fromIntegral idx)
  in vm { vmStack = addStackValue (vmStack vm) local }
executeInstruction vmConf _ = vmConf

------------------------------

execFunction' :: VM -> Function -> VM
execFunction' vm (Function typeIdx funcIdx locals body)
  | instructionIdx vm >= fromIntegral (length body) = vm
  | otherwise = do
    let instruction = body !! (fromIntegral (instructionIdx vm))
    let newVm = trace ("Exe Instruciton: " ++ show instruction ++ " locals: " ++ show (vmLocals vm)) executeInstruction vm instruction
    execFunction' (incrementInstructionIdx newVm) (Function typeIdx funcIdx locals body)

execFunction :: VM -> Function -> VM
execFunction config (Function typeIdx funcIdx locals body) = do
  let vmConf = config { currentFunctionIdx = funcIdx, instructionIdx = 0}
  execFunction' (initLocals vmConf) (Function typeIdx funcIdx locals body)

getTestAddFunction :: Function
getTestAddFunction = Function 3 2 [Local 0 I32, Local 1 I32] [GetLocal 0, GetLocal 1, I32Add, End]

getMainTestFunction :: Function
getMainTestFunction = Function 0 1 [] [I32Const 10, I32Const 5, Call 2, End]

getFunctionsForWasmMod :: [Function]
getFunctionsForWasmMod = [getTestAddFunction, getMainTestFunction]

start :: WasmModule -> IO ()
start wasmMod = do
  let configVm = VM (Stack []) 0 0 [] [] (wasmMod { functions = getFunctionsForWasmMod })
  --let startFunc = getStartFunction (exports wasmMod) (functions wasmMod)
  let startFunc = getMainTestFunction
  let finalVMConf = execFunction configVm startFunc
  print (vmStack finalVMConf)
  return ()
