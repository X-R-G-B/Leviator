{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Types
-}

module Run.Types
  (
    VMConfig(..),
    Stack(..),
    StackValue(..),
    addStackValue,
    stackPop,
    stackDrop
  )
where

import Data.Int (Int32, Int64)
import Control.Exception (throw)

import Types
import Errors

------------------------- stack
data StackValue = I_32 Int32 | I_64 Int64 | F_32 Float | F_64 Double | Null deriving (Show, Eq)

data Stack = Stack [StackValue] deriving (Show)

addStackValue :: Stack -> StackValue -> Stack
addStackValue (Stack stack) value = Stack (value : stack)

stackPop :: Stack -> (StackValue, Stack)
stackPop (Stack []) = throw $ WasmError "stackPop: empty stack"
stackPop (Stack (x:xs)) = (x, Stack xs)

stackDrop :: Stack -> StackValue
stackDrop (Stack []) = throw $ WasmError "stackDrop: empty stack"
stackDrop (Stack (x:xs)) = x

------------------------- config

data VMConfig = VMConfig {
  vmStack :: Stack,
  instructionIdx :: Int32,
  currentFunctionIdx :: Int32,
  wasmModule :: WasmModule
} deriving (Show)
