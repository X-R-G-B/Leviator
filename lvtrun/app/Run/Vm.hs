{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Vm
-}

module Run.Vm
  (
    VM(..),
    startExecution,
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

import Debug.Trace

data Value =
  I_32 Int32
  | I_64 Int64
  | F_32 Float
  | F_64 Double
  deriving (Show)

type Stack = [Value]
type Locals = [Value]

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


----------------------------

getLocalFromId' :: Int32 -> LocalIdx -> Locals -> Value
getLocalFromId' _ _ [] = throw $ WasmError "getLocalFromId: bad id"
getLocalFromId' idx id (x:xs)
  | idx > id = throw $ WasmError "getLocalFromId: bad id"
  | idx == id = x
  | otherwise = getLocalFromId' (idx + 1) id xs

getLocalFromId :: CurrentExec -> LocalIdx -> Value
getLocalFromId cEx id = getLocalFromId' 0 id (ceLocals cEx)

setLocalWithId :: Int32 -> Locals -> Value -> LocalIdx -> Locals
setLocalWithId _ [] _ _ = throw $ WasmError "setLocalWithId: bad id"
setLocalWithId idx (x:xs) value id
  | idx > id = throw $ WasmError "setLocalWithId: bad id"
  | idx == id = value : xs
  | otherwise = x : setLocalWithId (idx + 1) xs value id

-- pushResults StackToPushTo StackToPopFrom ResultTypes
pushResults :: Stack -> Stack -> [TypeName] -> Stack
pushResults stack1 stack2 [] = stack1
pushResults stack1 stack2 ((I32):xs) = case stackTop stack2 of
  I_32 val -> pushResults (stackPush stack1 (I_32 val)) (tail stack2) xs
  _ -> throw $ WasmError "pushResults: bad type"
pushResults stack1 stack2 ((I64):xs) = case stackTop stack2 of
  I_64 val -> pushResults (stackPush stack1 (I_64 val)) (tail stack2) xs
  _ -> throw $ WasmError "pushResults: bad type"
pushResults stack1 stack2 ((F32):xs) = case stackTop stack2 of
  F_32 val -> pushResults (stackPush stack1 (F_32 val)) (tail stack2) xs
  _ -> throw $ WasmError "pushResults: bad type"
pushResults stack1 stack2 ((F64):xs) = case stackTop stack2 of
  F_64 val -> pushResults (stackPush stack1 (F_64 val)) (tail stack2) xs
  _ -> throw $ WasmError "pushResults: bad type"
pushResults stack1 stack2 _ = throw $ WasmError "pushResults: bad type"

stackPush :: Stack -> Value -> Stack
stackPush stack value = value:stack

stackPop :: Stack -> (Value, Stack)
stackPop [] = throw $ WasmError "stackPop: empty stack"
stackPop (x:xs) = (x, xs)

stackTop :: Stack -> Value
stackTop [] = throw $ WasmError "stackTop: empty stack"
stackTop (x:xs) = x

stackPopN :: Stack -> Int -> ([Value], Stack)
stackPopN stack 0 = ([], stack)
stackPopN stack n
  | n > 0 = do
      let (value, newStack) = stackPop stack
      let (values, finalStack) = stackPopN newStack (n - 1)
      (value : values, finalStack)
  | otherwise = error "stackPopN: bad n"

---------------------------

initLocalsVar :: Locals -> [Local] -> Locals
initLocalsVar newLocals [] = newLocals
initLocalsVar newLocals ((Local _ I32):xs) = initLocalsVar (I_32 0 : newLocals) xs
initLocalsVar newLocals ((Local _ I64):xs) = initLocalsVar (I_64 0 : newLocals) xs
initLocalsVar newLocals ((Local _ F32):xs) = initLocalsVar (F_32 0 : newLocals) xs
initLocalsVar newLocals ((Local _ F64):xs) = initLocalsVar (F_64 0 : newLocals) xs

createLocalsParams :: [TypeName] -> [Value] -> Locals
createLocalsParams [] [] = []
createLocalsParams (I32:xs) (I_32 val:xs2) = (I_32 val : createLocalsParams xs xs2)
createLocalsParams (I64:xs) (I_64 val:xs2) = (I_64 val : createLocalsParams xs xs2)
createLocalsParams (F32:xs) (F_32 val:xs2) = (F_32 val : createLocalsParams xs xs2)
createLocalsParams (F64:xs) (F_64 val:xs2) = (F_64 val : createLocalsParams xs xs2)
createLocalsParams _ _ = throw $ WasmError "createLocalsParams: bad type"

initLocalsParams :: [TypeName] -> Stack -> (Locals, Stack)
initLocalsParams [] stack = ([], stack)
initLocalsParams params stack 
  | length params > length stack = throw $ WasmError "initLocalsParam: bad nb"
  | otherwise = do
    let (values, newStack) = stackPopN stack (length params)
    let reversedValues = reverse values
    let newLocals = createLocalsParams params reversedValues
    (newLocals, newStack)

initLocals :: [Local] -> [TypeName] -> Stack -> (Locals, Stack)
initLocals localVarTypes paramTypes stack = do
  let (newLocals, newStack) = initLocalsParams paramTypes stack
  let localsVar = initLocalsVar newLocals localVarTypes
  (newLocals ++ localsVar, newStack)

---------------------------

createEmptyLocals :: Locals -> [Local] -> Locals
createEmptyLocals newLocals [] = newLocals
createEmptyLocals newLocals ((Local _ I32):xs) = createEmptyLocals (I_32 0 : newLocals) xs
createEmptyLocals newLocals ((Local _ I64):xs) = createEmptyLocals (I_64 0 : newLocals) xs
createEmptyLocals newLocals ((Local _ F32):xs) = createEmptyLocals (F_32 0 : newLocals) xs
createEmptyLocals newLocals ((Local _ F64):xs) = createEmptyLocals (F_64 0 : newLocals) xs

-- fillLocals :: [TypeName] -> [Value] -> Locals -> Locals
-- fillLocals [] [] locals = locals
-- fillLocals (I32:xs) (I_32 val:xs2) (_:locals) = (I_32 val : fillLocals xs xs2 locals)
-- fillLocals (I64:xs) (I_64 val:xs2) (_:locals) = (I_64 val : fillLocals xs xs2 locals)
-- fillLocals (F32:xs) (F_32 val:xs2) (_:locals) = (F_32 val : fillLocals xs xs2 locals)
-- fillLocals (F64:xs) (F_64 val:xs2) (_:locals) = (F_64 val : fillLocals xs xs2 locals)
-- fillLocals _ _ _ = throw $ WasmError "fillLocals: bad type"

-- initLocals :: Int -> [TypeName] -> Stack -> Locals -> (Locals, Stack)
-- initLocals nb types stack locals
--   | nb /= length types = throw $ WasmError "initLocals: bad nb"
--   | nb > length stack = throw $ WasmError "initLocals: bad nb"
--   | otherwise = do
--     let (values, newStack) = stackPopN stack nb
--     let reversedValues = reverse values
--     let newLocals = fillLocals types reversedValues locals
--     (newLocals, newStack)

---------------------------

addLabel :: CurrentExec -> CurrentExec
addLabel cEx = cEx { ceLabels = (ceLabels cEx) ++ [ceInstIdx cEx] }

incrementInstIdx :: CurrentExec -> CurrentExec
incrementInstIdx cEx = cEx { ceInstIdx = ceInstIdx cEx + 1 }

---------------------------

execOpCode :: VM -> CurrentExec -> Instruction -> CurrentExec
execOpCode vm cEx (I32Const val) = trace ("--i32const--" ++ show val) cEx { ceStack = stackPush (ceStack cEx) (I_32 val) }
execOpCode vm cEx (Block _) = trace "--block--" addLabel cEx { crBlockIndents = (crBlockIndents cEx) + 1 }
execOpCode vm cEx (I32Eqz) = do
  let value = trace "--i32eqz--" stackTop (ceStack cEx)
  case value of
    I_32 0 -> cEx { ceStack = stackPush (ceStack cEx) (I_32 1) }
    I_32 _ -> cEx { ceStack = stackPush (ceStack cEx) (I_32 0) }
    _ -> throw $ WasmError "exec I32eqz: bad type"
execOpCode vm cEx (I32Add) = do
  let (value2, newStack1) = trace "--i32add--" stackPop (ceStack cEx)
  let (value1, newStack2) = stackPop newStack1
  case (value1, value2) of
    (I_32 val1, I_32 val2) -> cEx { ceStack = stackPush newStack2 (I_32 (val1 + val2)) }
    _ -> throw $ WasmError "exec I32Add: bad type"
execOpCode vm cEx (I32Sub) = do
  let (value2, newStack1) = trace "--i32sub--" stackPop (ceStack cEx)
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
  let newVm = trace ("--call--" ++ show funcIdx) execFunctionWithIdx vm funcIdx (ceStack cEx)
  let newStack = pushResults (ceStack cEx) (vmStack newVm) (ceResults (currentExec newVm))
  cEx { ceStack = newStack }
execOpCode vm cEx (End) = trace "--end--" cEx { crBlockIndents = (crBlockIndents cEx) - 1 }
execOpCode vm cEx (Return) = trace "--end--" cEx { crBlockIndents = (crBlockIndents cEx) - 1 }
execOpCode vm cEx (Unreachable) = throw $ WasmError "execOpCode: unreachable"
execOpCode vm cEx (GetLocal localIdx) = do
  let value = trace ("--getLocal--" ++ show localIdx) getLocalFromId cEx localIdx
  cEx { ceStack = stackPush (ceStack cEx) value }
execOpCode vm cEx (SetLocal localIdx) = do
  let (value, newStack) = trace ("--setLocal--" ++ show localIdx) stackPop (ceStack cEx)
  let newLocals = setLocalWithId 0 (ceLocals cEx) value localIdx
  cEx { ceStack = newStack, ceLocals = newLocals }
execOpCode vm cEx _ = trace ("not for: " ++ show (ceInstIdx cEx)) cEx

execOpCodes :: VM -> [Instruction] -> CurrentExec
execOpCodes vm [] = currentExec vm
execOpCodes vm instructions
  | ceInstIdx cEx >= length instructions = cEx
  | ceInstIdx cEx < 0 = throw $ WasmError "execOpCodes: bad index"
  | (instructions !! ceInstIdx cEx) == End && crBlockIndents cEx == 0 = cEx
  | otherwise = do
    let newCEx = trace ("currentOpCode: " ++ show (instructions !! ceInstIdx cEx)) execOpCode vm cEx (instructions !! ceInstIdx cEx)
    let newVm = vm { currentExec = (incrementInstIdx newCEx) }
    execOpCodes newVm instructions
  where cEx = currentExec vm

execFunction :: VM -> VM
execFunction vm = do
  let newCEx = trace ("exex: " ++ show (ceInstructions (currentExec vm))) execOpCodes vm (ceInstructions (currentExec vm))
  vm { currentExec = newCEx, vmStack = (pushResults (vmStack vm) (ceStack newCEx) (ceResults newCEx)) }

execFunctionWithIdx :: VM -> FuncIdx -> Stack -> VM
execFunctionWithIdx vm funcIdx currentStack = do
  let function = getFunctionFromId funcIdx (functions (wasmModule vm))
  let funcTypee = getFuncTypeFromId (funcType function) (types (wasmModule vm))
  let (newLocals, newStack) = initLocals (locals function) (params funcTypee) currentStack
  let cexec = CurrentExec {
    ceLocals = trace ("ij" ++ show newLocals) newLocals,
    ceStack = newStack,
    ceInstructions = body function,
    ceInstIdx = 0,
    ceLabels = [],
    ceParams = params funcTypee,
    ceResults = results funcTypee,
    crBlockIndents = 0
  }
  execFunction vm { currentExec = cexec }

startExecution :: VM -> FuncIdx -> IO ()
startExecution vm funcIdx = do
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
  exitWith $ ExitSuccess
