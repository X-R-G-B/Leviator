{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Stack
-}

module Run.Stack
(
  Stack,
  pushResults,
  stackPush,
  stackPop,
  stackTop,
  stackPopN
)
where

import Control.Exception (throw)

import Types
import Errors

type Stack = [Value]

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
