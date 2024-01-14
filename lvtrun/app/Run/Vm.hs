{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Vm
-}

module Run.Vm
(
  VM(..),
  startExecution2,
  createVm
)
where

import Data.Int (Int32, Int64)
import Data.Word (Word8)
import Control.Exception (throw)
import System.Exit

import Types
import Errors
import Run.Functions
import Run.Stack
import Run.Locals

data CurrentExec = CurrentExec {
  ceLocals :: Locals,
  ceStack :: Stack,
  ceInstructions :: [Instruction],
  ceInstIdx :: Int,
  ceLabels :: [Int],
  ceParams :: [TypeName],
  ceResults :: [TypeName],
  crBlockIndents :: Int
} deriving (Show)

data InstMemory = Memory {
  memRange :: Limit,
  memData :: [Word8]
} deriving (Show)

data VM = VM {
  vmStack :: Stack,
  currentExec :: CurrentExec,
  vmMemory :: InstMemory,
  wasmModule :: WasmModule
}

instance Show VM where
  show vm = "VM { vmStack = " ++ show (vmStack vm) ++ ", currentExec = " ++ show (currentExec vm) ++ " }"

createVm :: WasmModule -> VM
createVm wasmMod = VM {
  vmStack = [],
  currentExec = CurrentExec {
    ceLocals = [],
    ceStack = [],
    ceInstructions = [],
    ceParams = [],
    ceResults = []
  },
  vmMemory = Memory {
    memRange = Limit 0 Nothing,
    memData = []
  },
  wasmModule = wasmMod
}

addLabel :: CurrentExec -> CurrentExec
addLabel cEx = cEx { ceLabels = (ceLabels cEx) ++ [ceInstIdx cEx] }

incrementInstIdx :: CurrentExec -> CurrentExec
incrementInstIdx cEx = cEx { ceInstIdx = ceInstIdx cEx + 1 }

---------------------------

execOpCode :: VM -> CurrentExec -> Instruction -> CurrentExec
execOpCode vm cEx (I32Const val) = cEx { ceStack = stackPush (ceStack cEx) (I_32 val) }
execOpCode vm cEx (Block _) = cEx { crBlockIndents = (crBlockIndents cEx) + 1 }
execOpCode vm cEx (I32Eqz) = do
  let value = stackTop (ceStack cEx)
  case value of
    I_32 0 -> cEx { ceStack = stackPush (ceStack cEx) (I_32 1) }
    I_32 _ -> cEx { ceStack = stackPush (ceStack cEx) (I_32 0) }
    _ -> throw $ WasmError "exec I32eqz: bad type"
execOpCode vm cEx (I32Add) = do
  let (value2, newStack1) = stackPop (ceStack cEx)
  let (value1, newStack2) = stackPop newStack1
  case (value1, value2) of
    (I_32 val1, I_32 val2) -> cEx { ceStack = stackPush newStack2 (I_32 (val1 + val2)) }
    _ -> throw $ WasmError "exec I32Add: bad type"
execOpCode vm cEx (I32Sub) = do
  let (value2, newStack1) = stackPop (ceStack cEx)
  let (value1, newStack2) = stackPop newStack1
  case (value1, value2) of
    (I_32 val1, I_32 val2) -> cEx { ceStack = stackPush newStack2 (I_32 (val1 - val2)) }
    _ -> throw $ WasmError "exec I32Sub: bad type"
execOpCode vm cEx (BrIf labelIdx) = do
  let (value, newStack) = stackPop (ceStack cEx)
  case value of
    I_32 0 -> cEx
    I_32 _ -> cEx { ceStack = newStack, ceInstIdx = (fromIntegral labelIdx) }
    _ -> throw $ WasmError "exec brIf: bad type"
execOpCode vm cEx (Call funcIdx) = do
  let newVm = execFunctionWithIdx vm funcIdx (ceStack cEx)
  let newStack = pushResults (ceStack cEx) (vmStack newVm) (ceResults (currentExec newVm))
  cEx { ceStack = newStack }
execOpCode vm cEx (End) = cEx { crBlockIndents = (crBlockIndents cEx) - 1 }
execOpCode vm cEx (Return) = cEx { crBlockIndents = (crBlockIndents cEx) - 1 }
execOpCode vm cEx (Unreachable) = throw $ WasmError "execOpCode: unreachable"
execOpCode vm cEx (GetLocal localIdx) = do
  let value = getLocalFromId (ceLocals cEx) localIdx
  cEx { ceStack = stackPush (ceStack cEx) value }
execOpCode vm cEx (SetLocal localIdx) = do
  let (value, newStack) = stackPop (ceStack cEx)
  let newLocals = setLocalWithId 0 (ceLocals cEx) value localIdx
  cEx { ceStack = newStack, ceLocals = newLocals }
execOpCode vm cEx _ = cEx

execOpCodes :: VM -> [Instruction] -> CurrentExec
execOpCodes vm [] = currentExec vm
execOpCodes vm instructions
  | ceInstIdx cEx >= length instructions = cEx
  | ceInstIdx cEx < 0 = throw $ WasmError "execOpCodes: bad index"
  | (instructions !! ceInstIdx cEx) == End && crBlockIndents cEx == 0 = cEx
  | otherwise = do
    let newCEx = execOpCode vm cEx (instructions !! ceInstIdx cEx)
    let newVm = vm { currentExec = (incrementInstIdx newCEx) }
    execOpCodes newVm instructions
  where cEx = currentExec vm

execFunction :: VM -> VM
execFunction vm = do
  let newCEx = execOpCodes vm (ceInstructions (currentExec vm))
  vm { currentExec = newCEx, vmStack = (pushResults (vmStack vm) (ceStack newCEx) (ceResults newCEx)) }

execFunctionWithIdx :: VM -> FuncIdx -> Stack -> VM
execFunctionWithIdx vm funcIdx currentStack = do
  let function = getFunctionFromId funcIdx (functions (wasmModule vm))
  let funcTypee = getFuncTypeFromId (funcType function) (types (wasmModule vm))
  let (newLocals, newStack) = initLocals (locals function) (params funcTypee) currentStack
  let cexec = CurrentExec {
    ceLocals = newLocals,
    ceStack = newStack,
    ceInstructions = body function,
    ceInstIdx = 0,
    ceLabels = [],
    ceParams = params funcTypee,
    ceResults = results funcTypee,
    crBlockIndents = 0
  }
  execFunction vm { currentExec = cexec }

startExecution2 :: VM -> FuncIdx -> IO ()
startExecution2 vm funcIdx = do
  let function = getFunctionFromId funcIdx (functions (wasmModule vm))
  let funcTypee = getFuncTypeFromId (funcType function) (types (wasmModule vm))
  let cexec = CurrentExec {
    ceLocals = createEmptyLocals [] (locals function),
    ceStack = [],
    ceInstructions = body function,
    ceInstIdx = 0,
    ceLabels = [],
    ceParams = params funcTypee,
    ceResults = results funcTypee,
    crBlockIndents = 0
  }
  let newVm = execFunction vm { currentExec = cexec }
  let resStack = []
  let res = pushResults resStack (vmStack newVm) (ceResults (currentExec newVm))
  let exitCode = case res of
        [] -> 0
        (x:xs) -> case x of
          I_32 val -> val
          _ -> 0
  putStrLn $ "Exit correctly with code: " ++ show exitCode
