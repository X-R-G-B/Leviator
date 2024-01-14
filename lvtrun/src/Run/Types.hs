{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Types
-}

module Run.Types
(
  CurrentExec(..),
  InstMemory(..),
  VM(..),
  createVm,
  incrementInstIdx
)
where

import Data.Word (Word8)

import Types
import Run.Stack (Stack)
import Run.Locals (Locals)

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
} deriving (Show)

createVm :: WasmModule -> VM
createVm wasmMod = VM { vmStack = [],
  currentExec = CurrentExec { ceLocals = [],
    ceStack = [], ceInstructions = [],
    ceParams = [], ceResults = [], ceInstIdx = 0,
    ceLabels = [], crBlockIndents = 0},
  vmMemory = Memory {
    memRange = Limit 0 Nothing, memData = []
  },
  wasmModule = wasmMod
}

incrementInstIdx :: CurrentExec -> CurrentExec
incrementInstIdx cEx = cEx { ceInstIdx = ceInstIdx cEx + 1 }
