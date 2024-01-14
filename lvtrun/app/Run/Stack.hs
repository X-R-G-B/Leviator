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

import Types (Value(..), TypeName(..))
import Errors (CustomException(..))

type Stack = [Value]

pushResults :: Stack -> Stack -> [TypeName] -> Stack
pushResults toStack _ [] = toStack
pushResults toStack fromStack ((I32):xs) =
  case stackTop fromStack of
    I_32 val -> pushResults (stackPush toStack (I_32 val)) (tail fromStack) xs
    _ -> throw $ RuntimeError "pushResults: bad type"
pushResults toStack fromStack ((I64):xs) =
  case stackTop fromStack of
    I_64 val -> pushResults (stackPush toStack (I_64 val)) (tail fromStack) xs
    _ -> throw $ RuntimeError "pushResults: bad type"
pushResults toStack fromStack ((F32):xs) =
  case stackTop fromStack of
    F_32 val -> pushResults (stackPush toStack (F_32 val)) (tail fromStack) xs
    _ -> throw $ RuntimeError "pushResults: bad type"
pushResults toStack fromStack ((F64):xs) =
  case stackTop fromStack of
    F_64 val -> pushResults (stackPush toStack (F_64 val)) (tail fromStack) xs
    _ -> throw $ RuntimeError "pushResults: bad type"

stackPush :: Stack -> Value -> Stack
stackPush stack value = value:stack

stackPop :: Stack -> (Value, Stack)
stackPop [] = throw $ RuntimeError "stackPop: empty stack"
stackPop (x:xs) = (x, xs)

stackTop :: Stack -> Value
stackTop [] = throw $ RuntimeError "stackTop: empty stack"
stackTop (x:_) = x

stackPopN :: Stack -> Int -> ([Value], Stack)
stackPopN stack 0 = ([], stack)
stackPopN stack n
  | n > 0 = do
    let (value, newStack) = stackPop stack
    let (values, finalStack) = stackPopN newStack (n - 1)
    (value : values, finalStack)
  | otherwise = error "stackPopN: bad n"
