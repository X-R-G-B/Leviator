{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Vm
-}

module Run.Vm
(
  VM(..),
  runMain,
  createVm
)
where

import Data.Word (Word8)
import Control.Exception (throw)

import Types
import Run.Types
import Run.Locals
import Errors (CustomException(..))
import Run.Functions (getFunctionFromId, getFuncTypeFromId)
import Run.Stack (Stack, stackPush, stackPop, stackTop, pushResults)

goToEndInstruction :: CurrentExec -> CurrentExec
goToEndInstruction cexec
  | ceInstIdx cexec >= length (ceInstructions cexec) =
    throw $ RuntimeError "goToEndInstruction: missing end instruction"
  | currentOpCode == End = cexec { ceInstIdx = (ceInstIdx cexec) }
  | otherwise = goToEndInstruction (incrementInstIdx cexec)
  where
    currentOpCode = (ceInstructions cexec) !! (ceInstIdx cexec)

execOpCode :: VM -> CurrentExec -> Instruction -> CurrentExec
execOpCode _ cEx (I32Const val) = cEx { ceStack = stackPush (ceStack cEx) (I_32 val) }
execOpCode _ cEx (Block _) = cEx { crBlockIndents = (crBlockIndents cEx) + 1 }
execOpCode _ cEx (I32Eqz) = do
  let value = stackTop (ceStack cEx)
  case value of
    I_32 0 -> cEx { ceStack = stackPush (ceStack cEx) (I_32 1) }
    I_32 _ -> cEx { ceStack = stackPush (ceStack cEx) (I_32 0) }
    _ -> throw $ RuntimeError "exec I32eqz: bad type"
execOpCode _ cEx (I32Eq) = do
  let (value2, newStack1) = stackPop (ceStack cEx)
  let (value1, newStack2) = stackPop newStack1
  case (value1, value2) of
    (I_32 val1, I_32 val2) -> cEx { ceStack = stackPush newStack2 (I_32 (if val1 == val2 then 1 else 0)) }
    _ -> throw $ RuntimeError "exec I32Eq: bad type"
execOpCode _ cEx (I32Add) = do
  let (value2, newStack1) = stackPop (ceStack cEx)
  let (value1, newStack2) = stackPop newStack1
  case (value1, value2) of
    (I_32 val1, I_32 val2) -> cEx { ceStack = stackPush newStack2 (I_32 (val1 + val2)) }
    _ -> throw $ RuntimeError "exec I32Add: bad type"
execOpCode _ cEx (I32Sub) = do
  let (value2, newStack1) = stackPop (ceStack cEx)
  let (value1, newStack2) = stackPop newStack1
  case (value1, value2) of
    (I_32 val1, I_32 val2) -> cEx { ceStack = stackPush newStack2 (I_32 (val1 - val2)) }
    _ -> throw $ RuntimeError "exec I32Sub: bad type"
execOpCode _ cEx (I32Mul) = do
  let (value2, newStack1) = stackPop (ceStack cEx)
  let (value1, newStack2) = stackPop newStack1
  case (value1, value2) of
    (I_32 val1, I_32 val2) -> cEx { ceStack = stackPush newStack2 (I_32 (val1 * val2)) }
    _ -> throw $ RuntimeError "exec I32Mul: bad type"
execOpCode _ cEx (I32Divs) = do
  let (value2, newStack1) = stackPop (ceStack cEx)
  let (value1, newStack2) = stackPop newStack1
  case (value1, value2) of
    (I_32 _, I_32 0) -> throw $ RuntimeError "exec I32Divs: division by zero"
    (I_32 val1, I_32 val2) -> cEx { ceStack = stackPush newStack2 (I_32 (val1 `div` val2)) }
    _ -> throw $ RuntimeError "exec I32Divs: bad type"
execOpCode _ cEx (BrIf labelIdx) = case stackTop (ceStack cEx) of
    I_32 0 -> cEx
    I_32 _ -> cEx { ceInstIdx = (fromIntegral labelIdx) }
    _ -> throw $ RuntimeError "exec brIf: bad type"
execOpCode vm cEx (Call funcIdx) = do
  let newVm = execFunctionWithIdx vm funcIdx (ceStack cEx)
  let newStack = pushResults (ceStack cEx) (vmStack newVm) (ceResults (currentExec newVm))
  cEx { ceStack = newStack }
execOpCode _ cEx (End) = cEx { crBlockIndents = (crBlockIndents cEx) - 1 }
execOpCode _ cEx (Return) = cEx { crBlockIndents = (crBlockIndents cEx) - 1 }
execOpCode _ cEx (Unreachable) = throw $ RuntimeError "execOpCode: unreachable"
execOpCode _ cEx (GetLocal localIdx) = do
  let value = getLocalFromId (ceLocals cEx) localIdx
  cEx { ceStack = stackPush (ceStack cEx) value }
execOpCode _ cEx (SetLocal localIdx) = do
  let (value, newStack) = stackPop (ceStack cEx)
  let newLocals = setLocalWithId 0 (ceLocals cEx) value localIdx
  cEx { ceStack = newStack, ceLocals = newLocals }
execOpCode _ cEx (If) = case stackTop (ceStack cEx) of
    I_32 0 -> goToEndInstruction cEx
    I_32 1 -> cEx { crBlockIndents = (crBlockIndents cEx) + 1 }
    I_32 _ -> throw $ RuntimeError "execOpCode: bad if statement"
    _ -> throw $ RuntimeError "execOpCode: bad type"
execOpCode _ cEx _ = cEx

execOpCodes :: VM -> [Instruction] -> CurrentExec
execOpCodes vm [] = currentExec vm
execOpCodes vm instructions
  | ceInstIdx cEx >= length instructions = cEx
  | ceInstIdx cEx < 0 = throw $ RuntimeError "execOpCodes: bad index"
  | (instructions !! ceInstIdx cEx) == End && crBlockIndents cEx == 0 = cEx
  | (instructions !! ceInstIdx cEx) == Return = cEx { ceInstIdx = (length instructions) }
  | otherwise = do
    let newCEx = execOpCode vm cEx (instructions !! ceInstIdx cEx)
    let newVm = vm { currentExec = (incrementInstIdx newCEx) }
    execOpCodes newVm instructions
  where cEx = currentExec vm

execFunction :: VM -> VM
execFunction vm = vm { currentExec = newCEx, vmStack = stackWithRes }
  where
    newCEx = execOpCodes vm (ceInstructions (currentExec vm))
    stackWithRes = pushResults (vmStack vm) (ceStack newCEx)
      (ceResults newCEx)

execFunctionWithIdx :: VM -> FuncIdx -> Stack -> VM
execFunctionWithIdx vm funcIdx currentStack =
  execFunction vm { currentExec = cexec }
  where
    function = getFunctionFromId funcIdx (functions (wasmModule vm))
    funcTypee = getFuncTypeFromId (funcType function) (types (wasmModule vm))
    (newLocals, newStack) =
      initLocals (locals function) (params funcTypee) currentStack
    cexec = createEmptyExec {
      ceLocals = newLocals, ceStack = newStack, ceInstructions = body function,
      ceParams = params funcTypee, ceResults = results funcTypee}

runMain :: VM -> FuncIdx -> Stack
runMain vm funcIdx =
  pushResults [] (vmStack newVm) (ceResults (currentExec newVm))
  where
    function = getFunctionFromId funcIdx (functions (wasmModule vm))
    funcTypee = getFuncTypeFromId (funcType function) (types (wasmModule vm))
    cexec = createEmptyExec {
      ceLocals = createEmptyLocals [] (locals function),
      ceInstructions = body function,
      ceParams = params funcTypee,
      ceResults = results funcTypee
    }
    newVm = execFunction vm { currentExec = cexec }
