{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- Global
-}

module Parsing.Global
  (
    getGlobals,
  )
where

import qualified Data.ByteString.Lazy as BSL
import Control.Exception (throw)
import Control.Monad (when)
import Data.Word (Word8)
import Data.Int (Int64)

import Leb128
import Types
import Errors

extractOpCode :: BSL.ByteString -> ([Word8], Int64, BSL.ByteString)
extractOpCode bytes
  | (head $ BSL.unpack bytes) == 0x00 = ([0x00], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x01 = ([0x01], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x0f = ([0x0f], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x10 = ([0x10], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x41 = ([0x41], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x42 = ([0x42], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x43 = ([0x43], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x44 = ([0x44], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x28 = ([0x28], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x29 = ([0x29], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x36 = ([0x36], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x37 = ([0x37], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x37 = ([0x37], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x20 = ([0x20], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x21 = ([0x21], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x23 = ([0x23], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x24 = ([0x24], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x6a = ([0x6a], 1, BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x6b = ([0x6b], 1, BSL.drop 1 bytes)
  | (BSL.unpack $ BSL.take 2 bytes) == [0x3f, 0x00] = ([0x3f, 0x00], 2, BSL.drop 2 bytes)
  | (BSL.unpack $ BSL.take 2 bytes) == [0x40, 0x00] = ([0x40, 0x00], 2, BSL.drop 2 bytes)
  | otherwise = throw $ WasmError "ExtractOpCode: bad opcode"

createInstruction :: OpCode -> BSL.ByteString -> (Instruction, BSL.ByteString)
createInstruction [0x01] bytes = (Unreachable, bytes)
createInstruction [0x01] bytes = (Nop, bytes)
createInstruction [0x0f] bytes = (Return, bytes)
createInstruction [0x6a] bytes = (I32Add, bytes)
createInstruction [0x6b] bytes = (I32Sub, bytes)
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
  (I32Load (MemArg align offset), rest2)
createInstruction [0x29] bytes = do
  let (align, rest) = extractLEB1282 bytes
  let (offset, rest2) = extractLEB1282 rest
  (I64Load (MemArg align offset), rest2)
createInstruction [0x36] bytes = do
  let (align, rest) = extractLEB1282 bytes
  let (offset, rest2) = extractLEB1282 rest
  (I32Store (MemArg align offset), rest2)
createInstruction [0x37] bytes = do
  let (align, rest) = extractLEB1282 bytes
  let (offset, rest2) = extractLEB1282 rest
  (I64Store (MemArg align offset), rest2)
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
createInstruction _ _ = throw $ WasmError "CreateInstruction: bad instruction"

parseInstruction :: BSL.ByteString -> (Instruction, BSL.ByteString)
parseInstruction bytes
  | BSL.length bytes == 0 = throw $ WasmError "ParseInstruction: no instruction"
  | otherwise = do
    let (opCode, nbParams, rest) = extractOpCode bytes
    let (instruction, rest2) = createInstruction opCode rest
    (instruction, rest2)

parseInstructions :: BSL.ByteString -> [Instruction]
parseInstructions bytes
  | BSL.length bytes == 0 = []
  | head (BSL.unpack bytes) == 0x0b = []
  | otherwise = do
    let (instruction, rest) = parseInstruction bytes
    instruction : parseInstructions rest

parseMutability :: Word8 -> Mutability
parseMutability 0x00 = Const
parseMutability 0x01 = Var
parseMutability _ = throw $ WasmError "ParseMutability: bad mutability"

getHexaIndex :: BSL.ByteString -> Int64 -> Int64
getHexaIndex content idx
  | idx >= (fromIntegral $ BSL.length content) = throw $ WasmError "GetHexaIndex: no 0x0b found"
  | (head $ BSL.unpack $ BSL.drop (fromIntegral idx) content) == 0x0b = idx
  | otherwise = getHexaIndex content (idx + 1)

extractExpression :: BSL.ByteString -> (BSL.ByteString, BSL.ByteString)
extractExpression content = do
    let idx = getHexaIndex content 0
    let expression = BSL.take (fromIntegral (idx + 1)) content
    let rest = BSL.drop (fromIntegral (idx + 1)) content
    (expression, rest)

parseGlobal :: BSL.ByteString -> (Global, BSL.ByteString)
parseGlobal content = do
    let globalType = getTypeFromByte (head $ BSL.unpack content)
    let mutability = parseMutability (head $ BSL.unpack $ BSL.drop 1 content)
    let (expression, rest) = extractExpression (BSL.drop 2 content)
    let instructions = parseInstructions expression
    (Global globalType mutability instructions, rest)

parseGlobals :: Int64 -> Int64 -> BSL.ByteString -> [Global]
parseGlobals idx maxIdx content
  | idx >= maxIdx = []
  | otherwise = do
    let (global, rest) = parseGlobal content
    global : parseGlobals (idx + 1) maxIdx rest

getGlobals :: Section -> [Global]
getGlobals (Section GlobalID _ content) = do
  let (vecSize, rest) = extractLEB128 content
  parseGlobals 0 vecSize rest
getGlobals _ = throw $ WasmError "getGlobals: bad section"
