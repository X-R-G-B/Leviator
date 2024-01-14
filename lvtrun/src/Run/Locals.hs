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
import Errors (CustomException(..))
import Run.Stack (Stack, stackPopN)

type Locals = [Value]

getLocalFromId' :: Int32 -> LocalIdx -> Locals -> Value
getLocalFromId' _ _ [] = throw $ RuntimeError "getLocalFromId: bad id"
getLocalFromId' idx idntifier (x:xs)
  | idx > idntifier = throw $ RuntimeError "getLocalFromId: bad id"
  | idx == idntifier = x
  | otherwise = getLocalFromId' (idx + 1) idntifier xs

getLocalFromId :: Locals -> LocalIdx -> Value
getLocalFromId lcals idntifier = getLocalFromId' 0 idntifier lcals

setLocalWithId :: Int32 -> Locals -> Value -> LocalIdx -> Locals
setLocalWithId _ [] _ _ = throw $ RuntimeError "setLocalWithId: bad id"
setLocalWithId idx (x:xs) value idntifier
  | idx > idntifier = throw $ RuntimeError "setLocalWithId: bad id"
  | idx == idntifier = value : xs
  | otherwise = x : setLocalWithId (idx + 1) xs value idntifier

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
createLocalsParams _ _ = throw $ RuntimeError "createLocalsParams: bad type"

initLocalsParams' :: (Locals, Stack) -> [TypeName] -> (Locals, Stack)
initLocalsParams' ([], newStack) _ = ([], newStack)
initLocalsParams' (values, newStack) prms =
  (createLocalsParams prms (reverse values), newStack)

initLocalsParams :: [TypeName] -> Stack -> (Locals, Stack)
initLocalsParams [] stack = ([], stack)
initLocalsParams prms stack 
  | length prms > length stack = throw $ RuntimeError "initLocalsParam: bad nb"
  | otherwise = initLocalsParams' (stackPopN stack (length prms)) prms

initLocals :: [Local] -> [TypeName] -> Stack -> (Locals, Stack)
initLocals localVarTypes paramTypes stack = (newLocals ++ localsVar, newStack)
  where
    (newLocals, newStack) = initLocalsParams paramTypes stack
    localsVar = initLocalsVar newLocals localVarTypes

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
