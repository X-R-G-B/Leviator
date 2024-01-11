{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Code
-}

module Run.Start
  (
    start
  )
where

import Data.Int (Int32)
import Control.Exception (throw)

import Types
import Errors

--------------------------------------

getStartFunctionId :: [Export] -> Int32
getStartFunctionId [] = throw $ WasmError "No start function"
getStartFunctionId (x:xs)
  | expName x == "_start" =
    case expDesc x of
      ExportFunc idx -> idx
      _ -> throw $ WasmError "getStartFunctionId: bad export"
  | otherwise = getStartFunctionId xs
getStartFunctionId _ = throw $ WasmError "getStartFunctionId: bad export"

getFunctionFromId :: Int32 -> [Function] -> Function
getFunctionFromId id [] = throw $ WasmError "getFunctionFromId: bad id"
getFunctionFromId id (x:xs)
  | funcIdx x == id = x
  | otherwise = getFunctionFromId id xs

getStartFunction :: [Export] -> [Function] -> Function
getStartFunction exports functions =
  getFunctionFromId (getStartFunctionId exports) functions

  -------------------------

execStartFunction :: Function -> IO ()
execStartFunction (Function typeIdx funcIdx locals body) = do
  print "----execStartFunction---"

start :: WasmModule -> IO ()
start wasmMod = do
  let startFunc = getStartFunction (exports wasmMod) (functions wasmMod)
  print startFunc
  execStartFunction startFunc
  return ()
