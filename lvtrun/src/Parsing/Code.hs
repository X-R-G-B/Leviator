{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Code
-}

module Parsing.Code
(
  getFuncCode,
)
where

import qualified Data.ByteString.Lazy as BSL
import Control.Exception (throw)
import Data.Int (Int64)

import Leb128
import Types
import Errors
import OpCodes

diviseBytes :: BSL.ByteString -> [BSL.ByteString]
diviseBytes bytes
  | BSL.length bytes == 0 = []
  | otherwise = do
    let (size, rest) = getLEB128ToI64 bytes
    let (code, rest2) = BSL.splitAt size rest
    code : diviseBytes rest2

createLocal :: LocalIdx -> TypeName -> Local
createLocal idx typee = Local {lcIdx = idx, lcType = typee}

extractLocal :: Int64 -> BSL.ByteString -> ([Local], BSL.ByteString)
extractLocal id bytes
  | BSL.length bytes == 0 = throw $ WasmError "extractLocal: bad section"
  | otherwise = do
    let (nbOfThisType, rest) = getLEB128ToI64 bytes
    let typee = getTypeFromByte (head (BSL.unpack (BSL.take 1 rest)))
    let locals = map (\x -> createLocal (fromIntegral id) typee) [0..nbOfThisType - 1]
    (locals, BSL.drop 1 rest)

extractLocals :: Int64 -> Int64 -> BSL.ByteString -> ([Local], BSL.ByteString)
extractLocals id idMax bytes
  | id >= idMax = ([], bytes)
  | BSL.length bytes == 0 = ([], bytes)
  | otherwise = do
    let (local, rest) = extractLocal id bytes
    let (locals, rest2) = extractLocals (id + 1) idMax rest
    (local ++ locals, rest2)

-------------------------

parseInstruction :: BSL.ByteString -> (Instruction, BSL.ByteString)
parseInstruction bytes
  | BSL.length bytes == 0 = throw $ WasmError "ParseInstruction: no instruction"
  | otherwise = do
    let (opCode, rest) = extractOpCode bytes
    let (instruction, rest2) = createInstruction opCode rest
    (instruction, rest2)

extractCode :: BSL.ByteString -> [Instruction]
extractCode bytes
  | BSL.length bytes == 0 = []
  | otherwise = do
    let (instruction, rest) = parseInstruction bytes
    instruction : extractCode rest

parseFunction :: BSL.ByteString -> Function -> Function
parseFunction bytes func = do
  let (nbLocalsTypes, rest) = getLEB128ToI64 bytes
  let (locals, rest2) = extractLocals 0 nbLocalsTypes rest
  func {locals = locals, body = extractCode rest2}

parseFunctions :: [BSL.ByteString] -> [Function] -> [Function]
parseFunctions [] [] = []
parseFunctions [] _ = throw $ WasmError "parseFunctions: bad section"
parseFunctions _ [] = throw $ WasmError "parseFunctions: bad section"
parseFunctions (x:xs) (y:ys) = parseFunction x y : parseFunctions xs ys

getFuncCode :: Section -> [Function] -> [Function]
getFuncCode (Section CodeID _ content) functions = do
  let (nbFunc, rest) = getLEB128ToI64 content
  let funcCodes = diviseBytes rest
  if (fromIntegral nbFunc) /= length functions
    then throw $ WasmError "getFuncCode: bad section"
    else parseFunctions funcCodes functions
getFuncCode _ _ = throw $ WasmError "getFuncCode: bad section"
