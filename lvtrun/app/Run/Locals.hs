{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Locals
-}

module Run.Locals
(
  Locals,
  getLocalFromId,
  setLocalWithId,
  initLocals,
  createEmptyLocals
)
where

import Data.Int (Int32)
import Control.Exception (throw)

import Types
import Errors
import Run.Stack

type Locals = [Value]

getLocalFromId' :: Int32 -> LocalIdx -> Locals -> Value
getLocalFromId' _ _ [] = throw $ WasmError "getLocalFromId: bad id"
getLocalFromId' idx id (x:xs)
  | idx > id = throw $ WasmError "getLocalFromId: bad id"
  | idx == id = x
  | otherwise = getLocalFromId' (idx + 1) id xs

getLocalFromId :: Locals -> LocalIdx -> Value
getLocalFromId locals id = getLocalFromId' 0 id locals

setLocalWithId :: Int32 -> Locals -> Value -> LocalIdx -> Locals
setLocalWithId _ [] _ _ = throw $ WasmError "setLocalWithId: bad id"
setLocalWithId idx (x:xs) value id
  | idx > id = throw $ WasmError "setLocalWithId: bad id"
  | idx == id = value : xs
  | otherwise = x : setLocalWithId (idx + 1) xs value id

----------- INITIALISATION ----------------

initLocalsVar :: Locals -> [Local] -> Locals
initLocalsVar newLocals [] = newLocals
initLocalsVar newLocals ((Local _ I32):xs) =
  initLocalsVar (I_32 0 : newLocals) xs
initLocalsVar newLocals ((Local _ I64):xs) =
  initLocalsVar (I_64 0 : newLocals) xs
initLocalsVar newLocals ((Local _ F32):xs) =
  initLocalsVar (F_32 0 : newLocals) xs
initLocalsVar newLocals ((Local _ F64):xs) =
  initLocalsVar (F_64 0 : newLocals) xs

createLocalsParams :: [TypeName] -> [Value] -> Locals
createLocalsParams [] [] = []
createLocalsParams (I32:xs) (I_32 val:xs2) =
  (I_32 val : createLocalsParams xs xs2)
createLocalsParams (I64:xs) (I_64 val:xs2) =
  (I_64 val : createLocalsParams xs xs2)
createLocalsParams (F32:xs) (F_32 val:xs2) =
  (F_32 val : createLocalsParams xs xs2)
createLocalsParams (F64:xs) (F_64 val:xs2) =
  (F_64 val : createLocalsParams xs xs2)
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

createEmptyLocals :: Locals -> [Local] -> Locals
createEmptyLocals newLocals [] = newLocals
createEmptyLocals newLocals ((Local _ I32):xs) =
  createEmptyLocals (I_32 0 : newLocals) xs
createEmptyLocals newLocals ((Local _ I64):xs) =
  createEmptyLocals (I_64 0 : newLocals) xs
createEmptyLocals newLocals ((Local _ F32):xs) =
  createEmptyLocals (F_32 0 : newLocals) xs
createEmptyLocals newLocals ((Local _ F64):xs) =
  createEmptyLocals (F_64 0 : newLocals) xs
