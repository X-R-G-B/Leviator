{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Functions
-}

module Run.Functions
(
  getStartFunctionId,
  getFunctionFromId,
  getStartFunction,
  getFuncTypeFromId
)
where

import Data.Int (Int32)
import Control.Exception (throw)

import Errors (CustomException(..))
import Types (Export(..), ExportDesc(..), Function(..), FuncType(..))

getStartFunctionId :: [Export] -> Int32
getStartFunctionId [] = throw $ RuntimeError "No start function"
getStartFunctionId (x:xs)
  | expName x == "start" =
    case expDesc x of
      ExportFunc idx -> idx
      _ -> throw $ RuntimeError "getStartFunctionId: bad export"
  | otherwise = getStartFunctionId xs

getFunctionFromId :: Int32 -> [Function] -> Function
getFunctionFromId _ [] = throw $ RuntimeError "getFunctionFromId: bad id"
getFunctionFromId idtfier (x:xs)
  | funcIdx x == idtfier = x
  | otherwise = getFunctionFromId idtfier xs

getStartFunction :: [Export] -> [Function] -> Function
getStartFunction exports functions =
  getFunctionFromId (getStartFunctionId exports) functions

getFuncTypeFromId :: Int32 -> [FuncType] -> FuncType
getFuncTypeFromId _ [] = throw $ RuntimeError "getFuncTypeFromId: bad id"
getFuncTypeFromId idtfier (x:xs)
  | typeId x == idtfier = x
  | otherwise = getFuncTypeFromId idtfier xs
