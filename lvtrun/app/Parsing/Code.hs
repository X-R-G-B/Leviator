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
import Control.Monad (when)
import Data.Word (Word8)
import Data.Int (Int64, Int32)
import Numeric (showHex)

import Leb128
import Types
import Errors

import Debug.Trace

-- GET LOCALS

diviseBytes :: BSL.ByteString -> [BSL.ByteString]
diviseBytes bytes
  | BSL.length bytes == 0 = []
  | otherwise = do
    let (size, rest) = extractLEB128 bytes
    let (code, rest2) = BSL.splitAt (fromIntegral size) rest
    code : diviseBytes rest2

createLocal :: LocalIdx -> TypeName -> Local
createLocal idx typee = Local {lcIdx = idx, lcType = typee}

extractLocal :: Int64 -> BSL.ByteString -> ([Local], BSL.ByteString)
extractLocal id bytes
  | BSL.length bytes == 0 = throw $ WasmError "extractLocal: bad section"
  | otherwise = do
    let (nbOfThisType, rest) = extractLEB1282 bytes
    let typee = getTypeFromByte (head (BSL.unpack (BSL.take 1 rest)))
    let locals = map (\x -> createLocal (fromIntegral id) typee) [0..(fromIntegral nbOfThisType - 1)]
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

extractOpCode :: BSL.ByteString -> ([Word8], Int64, BSL.ByteString)
extractOpCode bytes
  | (head $ BSL.unpack bytes) == 0x03 = ([0x00], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x11 = ([0x00], 1, BSL.drop 3 bytes)

  | (head $ BSL.unpack bytes) == 0x00 = ([0x00], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x0b = ([0x0b], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x0d = ([0x0d], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x0c = ([0x0c], 1, BSL.drop 2 bytes)
  | (head $ BSL.unpack bytes) == 0x02 = ([0x02], 1, BSL.drop 2 bytes)

  | (head $ BSL.unpack bytes) == 0x01 = ([0x01], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x0f = ([0x0f], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x10 = ([0x10], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x41 = ([0x41], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x42 = ([0x42], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x43 = ([0x43], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x44 = ([0x44], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x28 = ([0x28], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x29 = ([0x29], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x22 = ([0x22], 1, BSL.drop 2 bytes)
  | (head $ BSL.unpack bytes) == 0x36 = ([0x36], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x37 = ([0x37], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x4b = ([0x4b], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x37 = ([0x37], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x20 = ([0x20], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x4d = ([0x4d], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x21 = ([0x21], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x23 = ([0x23], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x24 = ([0x24], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x6a = ([0x6a], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x6b = ([0x6b], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x45 = ([0x45], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x71 = ([0x00], 1, BSL.drop 1 bytes)
  | (BSL.unpack $ BSL.take 2 bytes) == [0x3f, 0x00] = ([0x3f, 0x00], 2, BSL.drop 2 bytes)
  | (BSL.unpack $ BSL.take 2 bytes) == [0x40, 0x00] = ([0x40, 0x00], 2, BSL.drop 2 bytes)
  | otherwise = throw $ WasmError "extractOpCode: bad instruction"

createInstruction :: OpCode -> BSL.ByteString -> (Instruction, BSL.ByteString)
createInstruction [0x03] bytes = (Nop, bytes)
createInstruction [0x11] bytes = (Nop, bytes)

createInstruction [0x00] bytes = (Unreachable, bytes)
createInstruction [0x01] bytes = (Nop, bytes)
createInstruction [0x02] bytes = (Block EmptyType, bytes)
createInstruction [0x0b] bytes = (End, bytes)
createInstruction [0x0f] bytes = (Return, bytes)
createInstruction [0x4b] bytes = (I32Gtu, bytes)
createInstruction [0x6a] bytes = (I32Add, bytes)
createInstruction [0x6b] bytes = (I32Sub, bytes)
createInstruction [0x45] bytes = (I32Eqz, bytes)
createInstruction [0x4d] bytes = (I32Leu, bytes)
createInstruction [0x71] bytes = (I32And, bytes)
createInstruction [0x0d] bytes = do
  let (value, rest) = extractLEB1282 bytes
  (BrIf value, rest)
createInstruction [0x0c] bytes = do
  let (value, rest) = extractLEB1282 bytes
  (Br value, rest)
createInstruction [0x22] bytes = do
  let (value, rest) = extractLEB1282 bytes
  (LocalTee value, rest)
createInstruction [0x10] bytes = do
  let (value, rest) = extractLEB1282 bytes
  (Call value, rest)
createInstruction [0x41] bytes = do
  let (value, rest) = extractLEB1282 bytes
  (I32Const value, rest)
createInstruction [0x42] bytes = do
  let (value, rest) = extractLEB128 bytes
  (I64Const value, rest)
createInstruction [0x43] bytes = do
  let (value, rest) = extractLEB128 bytes
  (F32Const (fromIntegral value), rest)
createInstruction [0x44] bytes = do
  let (value, rest) = extractLEB128 bytes
  (F64Const (fromIntegral value), rest)
createInstruction [0x28] bytes = do
  let (align, rest) = extractLEB1282 bytes
  let (offset, rest2) = extractLEB1282 rest
  (I32Load (MemArg offset align), rest2)
createInstruction [0x29] bytes = do
  let (align, rest) = extractLEB1282 bytes
  let (offset, rest2) = extractLEB1282 rest
  (I64Load (MemArg offset align), rest2)
createInstruction [0x36] bytes = do
  let (align, rest) = extractLEB1282 bytes
  let (offset, rest2) = extractLEB1282 rest
  (I32Store (MemArg offset align), rest2)
createInstruction [0x37] bytes = do
  let (align, rest) = extractLEB1282 bytes
  let (offset, rest2) = extractLEB1282 rest
  (I64Store (MemArg offset align), rest2)
createInstruction [0x20] bytes = do
  let (value, rest) = extractLEB1282 bytes
  (GetLocal value, rest)
createInstruction [0x24] bytes = do
  let (value, rest) = extractLEB1282 bytes
  (SetLocal value, rest)
createInstruction [0x23] bytes = do
  let (value, rest) = extractLEB1282 bytes
  (GetGlobal value, rest)
createInstruction [0x21] bytes = do
  let (value, rest) = extractLEB1282 bytes
  (SetGlobal value, rest)
createInstruction [0x3f, 0x00] bytes = (MemorySize, bytes)
createInstruction [0x40, 0x00] bytes = (MemoryGrow, bytes)
createInstruction opCode _ = trace ("createInstruction: " ++ show opCode) throw $ WasmError "createInstruction: bad instruction"

parseInstruction :: BSL.ByteString -> (Instruction, BSL.ByteString)
parseInstruction bytes
  | BSL.length bytes == 0 = throw $ WasmError "ParseInstruction: no instruction"
  | otherwise = do
    let (opCode, nbParams, rest) = extractOpCode bytes
    let (instruction, rest2) = createInstruction opCode rest
    (instruction, rest2)

extractCode :: BSL.ByteString -> [Instruction]
extractCode bytes
  | BSL.length bytes == 0 = []
  | otherwise = do
    let (instruction, rest) = parseInstruction bytes
    instruction : extractCode rest

------------------------

showBytes :: BSL.ByteString -> String
showBytes bytes = do
  let bytesList = BSL.unpack bytes
  let hexList = map (\x -> showHex x " ") bytesList
  foldl (\acc x -> acc ++ x) " " hexList

parseFunction :: BSL.ByteString -> Function -> Function
parseFunction bytes func = do
  let (nbLocalsTypes, rest) = extractLEB128 bytes
  let (locals, rest2) = extractLocals 0 nbLocalsTypes rest
  func {locals = locals, body = extractCode rest2}

parseFunctions :: [BSL.ByteString] -> [Function] -> [Function]
parseFunctions [] [] = []
parseFunctions [] _ = throw $ WasmError "parseFunctions: bad section"
parseFunctions _ [] = throw $ WasmError "parseFunctions: bad section"
parseFunctions (x:xs) (y:ys) = parseFunction x y : parseFunctions xs ys

getFuncCode :: Section -> [Function] -> [Function]
getFuncCode (Section CodeID _ content) functions = do
  let (nbFunc, rest) = extractLEB128 content
  let funcCodes = diviseBytes rest
  if (fromIntegral nbFunc) /= length functions
    then throw $ WasmError "getFuncCode: bad section"
    else parseFunctions funcCodes functions
getFuncCode _ _ = throw $ WasmError "getFuncCode: bad section"
