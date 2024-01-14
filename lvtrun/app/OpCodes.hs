{-
-- EPITECH PROJECT, 2023
-- Leviator Run
-- File description:
-- OpCodes
-}

module OpCodes
(
  extractOpCode,
  createInstruction
)
where

import qualified Data.ByteString.Lazy as BSL
import Control.Exception (throw)
import Data.Word (Word8)

import Leb128
import Types
import Errors

extractOpCode :: BSL.ByteString -> ([Word8], BSL.ByteString)
extractOpCode bytes
  | (head $ BSL.unpack bytes) == 0x03 = ([0x00], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x11 = ([0x00], BSL.drop 3 bytes)
  | (head $ BSL.unpack bytes) == 0x00 = ([0x00], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x0b = ([0x0b], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x0d = ([0x0d], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x0c = ([0x0c], BSL.drop 2 bytes)
  | (head $ BSL.unpack bytes) == 0x02 = ([0x02], BSL.drop 2 bytes)
  | (head $ BSL.unpack bytes) == 0x01 = ([0x01], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x0f = ([0x0f], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x10 = ([0x10], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x41 = ([0x41], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x42 = ([0x42], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x6c = ([0x6c], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x6d = ([0x6d], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x43 = ([0x43], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x44 = ([0x44], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x28 = ([0x28], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x29 = ([0x29], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x22 = ([0x22], BSL.drop 2 bytes)
  | (head $ BSL.unpack bytes) == 0x36 = ([0x36], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x37 = ([0x37], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x4b = ([0x4b], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x37 = ([0x37], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x20 = ([0x20], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x4d = ([0x4d], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x21 = ([0x21], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x23 = ([0x23], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x24 = ([0x24], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x6a = ([0x6a], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x6b = ([0x6b], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x45 = ([0x45], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x46 = ([0x46], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x71 = ([0x00], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x48 = ([0x48], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x4a = ([0x4a], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x4c = ([0x4c], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x4e = ([0x4e], BSL.drop 1 bytes)
  | (head $ BSL.unpack bytes) == 0x47 = ([0x47], BSL.drop 1 bytes)
  | (BSL.unpack $ BSL.take 2 bytes) == [0x3f, 0x00] = ([0x3f, 0x00], BSL.drop 2 bytes)
  | (BSL.unpack $ BSL.take 2 bytes) == [0x40, 0x00] = ([0x40, 0x00], BSL.drop 2 bytes)
  | otherwise = throw $ WasmError "ExtractOpCode2: bad opcode"

createInstruction :: OpCode -> BSL.ByteString -> (Instruction, BSL.ByteString)
createInstruction [0x03] bytes = (Nop, bytes)
createInstruction [0x11] bytes = (Nop, bytes)
createInstruction [0x00] bytes = (Unreachable, bytes)
createInstruction [0x01] bytes = (Nop, bytes)
createInstruction [0x02] bytes = (Block EmptyType, bytes)
createInstruction [0x0b] bytes = (End, bytes)
createInstruction [0x48] bytes = (I32Lts, bytes)
createInstruction [0x0f] bytes = (Return, bytes)
createInstruction [0x4b] bytes = (I32Gtu, bytes)
createInstruction [0x6a] bytes = (I32Add, bytes)
createInstruction [0x6c] bytes = (I32Mul, bytes)
createInstruction [0x6d] bytes = (I32Divs, bytes)
createInstruction [0x47] bytes = (I32Ne, bytes)
createInstruction [0x6b] bytes = (I32Sub, bytes)
createInstruction [0x4a] bytes = (I32Gts, bytes)
createInstruction [0x46] bytes = (I32Eqz, bytes)
createInstruction [0x45] bytes = (I32Eqz, bytes)
createInstruction [0x4d] bytes = (I32Leu, bytes)
createInstruction [0x4e] bytes = (I32Ges, bytes)
createInstruction [0x4c] bytes = (I32Les, bytes)
createInstruction [0x71] bytes = (I32And, bytes)
createInstruction [0x0d] bytes = do
  let (value, rest) = getLEB128ToI32 bytes
  (BrIf value, rest)
createInstruction [0x0c] bytes = do
  let (value, rest) = getLEB128ToI32 bytes
  (Br value, rest)
createInstruction [0x22] bytes = do
  let (value, rest) = getLEB128ToI32 bytes
  (LocalTee value, rest)
createInstruction [0x10] bytes = do
  let (value, rest) = getLEB128ToI32 bytes
  (Call value, rest)
createInstruction [0x41] bytes = do
  let (value, rest) = getLEB128ToI32 bytes
  (I32Const value, rest)
createInstruction [0x42] bytes = do
  let (value, rest) = getLEB128ToI64 bytes
  (I64Const value, rest)
createInstruction [0x43] bytes = do
  let (value, rest) = getLEB128ToI64 bytes
  (F32Const (fromIntegral value), rest)
createInstruction [0x44] bytes = do
  let (value, rest) = getLEB128ToI64 bytes
  (F64Const (fromIntegral value), rest)
createInstruction [0x28] bytes = do
  let (align, rest) = getLEB128ToI32 bytes
  let (offset, rest2) = getLEB128ToI32 rest
  (I32Load (MemArg offset align), rest2)
createInstruction [0x29] bytes = do
  let (align, rest) = getLEB128ToI32 bytes
  let (offset, rest2) = getLEB128ToI32 rest
  (I64Load (MemArg offset align), rest2)
createInstruction [0x36] bytes = do
  let (align, rest) = getLEB128ToI32 bytes
  let (offset, rest2) = getLEB128ToI32 rest
  (I32Store (MemArg offset align), rest2)
createInstruction [0x37] bytes = do
  let (align, rest) = getLEB128ToI32 bytes
  let (offset, rest2) = getLEB128ToI32 rest
  (I64Store (MemArg offset align), rest2)
createInstruction [0x20] bytes = do
  let (value, rest) = getLEB128ToI32 bytes
  (GetLocal value, rest)
createInstruction [0x24] bytes = do
  let (value, rest) = getLEB128ToI32 bytes
  (SetGlobal value, rest)
createInstruction [0x23] bytes = do
  let (value, rest) = getLEB128ToI32 bytes
  (GetGlobal value, rest)
createInstruction [0x21] bytes = do
  let (value, rest) = getLEB128ToI32 bytes
  (SetLocal value, rest)
createInstruction [0x3f, 0x00] bytes = (MemorySize, bytes)
createInstruction [0x40, 0x00] bytes = (MemoryGrow, bytes)
createInstruction _ _ = throw $ WasmError "createInstruction: bad instruction"
