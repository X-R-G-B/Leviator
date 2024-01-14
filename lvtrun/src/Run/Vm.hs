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

import Data.Int (Int32)
import Control.Exception (throw)

import Types
import Run.Types
import Run.Locals
import Errors (CustomException(..))
import Run.Functions (getFunctionFromId, getFuncTypeFromId)
import Run.Stack (Stack, stackPush, stackPop, stackTop, pushResults, stackPopN)

goToEndInstruction :: CurrentExec -> CurrentExec
goToEndInstruction cexec
  | ceInstIdx cexec >= length (ceInstructions cexec) =
    throw $ RuntimeError "goToEndInstruction: missing end instruction"
  | currentOpCode == End = cexec { ceInstIdx = (ceInstIdx cexec) }
  | otherwise = goToEndInstruction (incrementInstIdx cexec)
  where
    currentOpCode = (ceInstructions cexec) !! (ceInstIdx cexec)

execI32Const :: CurrentExec -> Int32 -> CurrentExec
execI32Const cEx val = cEx {ceStack = stackPush (ceStack cEx) (I_32 val)}

execI32Eqz :: CurrentExec -> CurrentExec
execI32Eqz cEx@(CurrentExec {ceStack = stack}) =
  case (stackTop stack) of
    I_32 0 -> cEx { ceStack = stackPush stack (I_32 1) }
    I_32 _ -> cEx { ceStack = stackPush stack (I_32 0) }
    _ -> throw $ RuntimeError "exec I32eqz: bad type"

execI32Add :: CurrentExec -> CurrentExec
execI32Add cEx@(CurrentExec {ceStack = stack}) =
  case (stackPopN stack 2) of
    ([I_32 val2, I_32 val1], newStack) ->
      cEx { ceStack = stackPush newStack (I_32 (val1 + val2)) }
    _ -> throw $ RuntimeError "exec I32add: bad type"

execI32Eq :: CurrentExec -> CurrentExec
execI32Eq cEx@(CurrentExec {ceStack = stack}) =
  case (stackPopN stack 2) of
    ([I_32 value2, I_32 value1], newStack) -> case (value1 == value2) of
      True -> cEx { ceStack = stackPush newStack (I_32 1) }
      False -> cEx { ceStack = stackPush newStack (I_32 0) }
    _ -> throw $ RuntimeError "exec I32Eq: bad type"

execI32Sub :: CurrentExec -> CurrentExec
execI32Sub cEx@(CurrentExec {ceStack = stack}) =
  case (stackPopN stack 2) of
    ([I_32 val2, I_32 val1], newStack) ->
      cEx { ceStack = stackPush newStack (I_32 (val1 - val2)) }
    _ -> throw $ RuntimeError "exec I32sub: bad type"

execI32Mul :: CurrentExec -> CurrentExec
execI32Mul cEx@(CurrentExec {ceStack = stack}) =
  case (stackPopN stack 2) of
    ([I_32 val2, I_32 val1], newStack) ->
      cEx { ceStack = stackPush newStack (I_32 (val1 * val2)) }
    _ -> throw $ RuntimeError "exec I32mul: bad type"

execI32Divs :: CurrentExec -> CurrentExec
execI32Divs cEx@(CurrentExec {ceStack = stack}) =
  case (stackPopN stack 2) of
    ([I_32 val2, I_32 val1], newStack) ->
      cEx { ceStack = stackPush newStack (I_32 (val1 `div` val2)) }
    _ -> throw $ RuntimeError "exec I32divs: bad type"

execGetLocal :: CurrentExec -> LocalIdx -> CurrentExec
execGetLocal cEx localIdx = cEx { ceStack =
    stackPush (ceStack cEx) (getLocalFromId (ceLocals cEx) localIdx)}

execSetLocal :: CurrentExec -> LocalIdx -> CurrentExec
execSetLocal cEx localIdx = cEx { ceStack = newStack,
  ceLocals = setLocalWithId 0 (ceLocals cEx) value localIdx}
  where (value, newStack) = stackPop (ceStack cEx)

execBrIf :: CurrentExec -> CurrentExec
execBrIf cEx@(CurrentExec {ceStack = stack}) =
  case (stackTop stack) of
    I_32 0 -> cEx
    I_32 _ -> cEx { ceInstIdx = (ceInstIdx cEx) }
    _ -> throw $ RuntimeError "exec brIf: bad type"

execCall :: VM -> CurrentExec -> FuncIdx -> CurrentExec
execCall vm cEx funcIdx = do
  let newVm = execFunctionWithIdx vm funcIdx (ceStack cEx)
  let newStack = pushResults (ceStack cEx) (vmStack newVm) (ceResults (currentExec newVm))
  cEx { ceStack = newStack }

execIf :: CurrentExec -> CurrentExec
execIf cEx@(CurrentExec {ceStack = stack}) = case stackTop stack of
  I_32 0 -> goToEndInstruction cEx
  I_32 1 -> cEx { crBlockIndents = (crBlockIndents cEx) + 1 }
  I_32 _ -> throw $ RuntimeError "execIf: bad if statement"
  _ -> throw $ RuntimeError "execIf: bad type"

execOpCode :: VM -> CurrentExec -> Instruction -> CurrentExec
execOpCode _ cEx (Unreachable) = throw $ RuntimeError "execOpCode: unreachable"
execOpCode _ cEx (End) = decrementBlockIdx cEx
execOpCode _ cEx (Return) = decrementBlockIdx cEx
execOpCode _ cEx (I32Const val) = execI32Const cEx val
execOpCode _ cEx (I32Eqz) = execI32Eqz cEx
execOpCode _ cEx (Block _) = cEx { crBlockIndents = (crBlockIndents cEx) + 1 }
execOpCode _ cEx (I32Eq) = execI32Eq cEx
execOpCode _ cEx (I32Add) = execI32Add cEx
execOpCode _ cEx (I32Sub) = execI32Sub cEx
execOpCode _ cEx (I32Mul) = execI32Mul cEx
execOpCode _ cEx (I32Divs) = execI32Divs cEx
execOpCode _ cEx (GetLocal localIdx) = execGetLocal cEx localIdx
execOpCode _ cEx (SetLocal localIdx) = execSetLocal cEx localIdx
execOpCode _ cEx (BrIf labelIdx) = execBrIf cEx
execOpCode vm cEx (Call funcIdx) = execCall vm cEx funcIdx
execOpCode _ cEx (If) = execIf cEx
execOpCode _ cEx _ = cEx

execOpCodes :: VM -> [Instruction] -> CurrentExec
execOpCodes vm [] = currentExec vm
execOpCodes vm instructions
  | ceInstIdx cEx >= length instructions = cEx
  | ceInstIdx cEx < 0 = throw $ RuntimeError "execOpCodes: bad index"
  | currentInst == End && crBlockIndents cEx == 0 = cEx
  | currentInst == Return = cEx { ceInstIdx = (length instructions) }
  | otherwise = execOpCodes newVm instructions
  where cEx = currentExec vm
        newCEx = execOpCode vm cEx (instructions !! ceInstIdx cEx)
        newVm = vm { currentExec = (incrementInstIdx newCEx) }
        currentInst = instructions !! ceInstIdx cEx

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
runMain vm funcIdx = pushResults[](vmStack newVm)(ceResults(currentExec newVm))
  where
    function = getFunctionFromId funcIdx (functions (wasmModule vm))
    funcTypee = getFuncTypeFromId (funcType function) (types (wasmModule vm))
    cexec = createEmptyExec {
      ceLocals = createEmptyLocals [] (locals function),
      ceInstructions = body function,
      ceParams = params funcTypee, ceResults = results funcTypee}
    newVm = execFunction vm { currentExec = cexec }
