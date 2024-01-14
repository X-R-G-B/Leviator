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
  incrementInstIdx,
  createEmptyExec,
  decrementBlockIdx,
  getLabelOpIdx,
  addLabel,
  incrementBlockIdx,
  goToLabel
)
where

import Data.Word (Word8)
import Control.Exception (throw)

import Types
import Data.Int (Int32)
import Run.Stack (Stack)
import Run.Locals (Locals)
import Errors (CustomException(..))

data CurrentExec = CurrentExec {
  ceLocals :: Locals,
  ceStack :: Stack,
  ceInstructions :: [Instruction],
  ceInstIdx :: Int,
  ceLabels :: [Int32],
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

goToLabel :: CurrentExec -> LabelIdx -> CurrentExec
goToLabel cEx labelIdx = cEx {ceInstIdx = fromIntegral (getLabelOpIdx cEx labelIdx)}

getLabelOpIdx :: CurrentExec -> LabelIdx -> Int
getLabelOpIdx cEx labelIdx
  | labelIdx >= fromIntegral (length (ceLabels cEx)) =
    throw $ RuntimeError "getLabelOpIdx: bad index"
  | otherwise = (fromIntegral (ceLabels cEx !! fromIntegral labelIdx))

doesLabelExist :: [Int32] -> LabelIdx -> Bool
doesLabelExist [] _ = False
doesLabelExist (x:xs) labelIdx
  | x == labelIdx = True
  | otherwise = doesLabelExist xs labelIdx

addLabel :: CurrentExec -> CurrentExec
addLabel cEx
  | doesLabelExist (ceLabels cEx) labelIdx = cEx
  | otherwise = cEx { ceLabels = (ceLabels cEx) ++ [labelIdx] }
  where
    labelIdx = fromIntegral (ceInstIdx cEx)

incrementInstIdx :: CurrentExec -> CurrentExec
incrementInstIdx cEx = cEx { ceInstIdx = ceInstIdx cEx + 1 }

incrementBlockIdx :: CurrentExec -> CurrentExec
incrementBlockIdx cEx = cEx { crBlockIndents = (crBlockIndents cEx) + 1 }

decrementBlockIdx :: CurrentExec -> CurrentExec
decrementBlockIdx cEx = cEx { crBlockIndents = (crBlockIndents cEx) - 1 }

createEmptyExec :: CurrentExec
createEmptyExec = CurrentExec {
  ceLocals = [],
  ceStack = [],
  ceInstructions = [],
  ceInstIdx = 0,
  ceLabels = [],
  ceParams = [],
  ceResults = [],
  crBlockIndents = 0
}
